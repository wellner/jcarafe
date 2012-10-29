/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import scala.math._
import scala.util.Random
import org.mitre.jcarafe.crf.ObsSource
import org.mitre.jcarafe.crf.StdModel
import org.mitre.jcarafe.crf.Alphabet
import org.mitre.jcarafe.crf.{InstanceSequence,AbstractInstance,SeqGen,PsaLearner,CondLogLikelihoodLearner,InstSeq}
import org.mitre.jcarafe.util._
import org.mitre.jcarafe.util.SLabel
import org.mitre.jcarafe.crf.IncrementalMurmurHash


class Evaluator(val opts: Options, val seqGen: MaxEntTrainingSeqGen) {
  def this() = this(new Options(), new MaxEntTrainingSeqGen(new Options()))

  import scala.collection.JavaConversions._

  import IncrementalMurmurHash._
  
  var instances : Option[InstanceSequence] = None

  def setInstances : Unit = { instances = Some(InstSeq(Random.shuffle(instances.get.iseq))) }

  def addInstances(i: InstanceSequence) = instances = Some(i)

  def addJInstance(l: String, fs: java.util.List[String]) : Unit = throw new RuntimeException("Not yet implemented")

  def getConfusionMatrix(s: Int, pairs: Seq[(Int,Int)]) : Array[Array[Int]] = {
    val matrix = Array.fill(s,s)(0)
    pairs foreach {case (a,b) => matrix(a)(b) += 1}
    matrix
  }

  private def getMe = {
    if (opts.parallel) {
	val numPs = opts.numThreads match {
          case None => Runtime.getRuntime.availableProcessors * 4/5 // leave a CPU or two free
          case Some(n) => n}
        println(">> Initiating Parallel Training using " + numPs + " processors <<\n")
        new DenseParallelMaxEnt(numPs, seqGen.getNumberOfStates,seqGen.getNumberOfFeatures,opts.gaussian)
      } else if (opts.psa) {
    	  new SparseMaxEnt(seqGen.getNumberOfStates,seqGen.getNumberOfFeatures,opts) with PsaLearner
      }
      else new MaxEnt(seqGen.getNumberOfStates,seqGen.getNumberOfFeatures,opts.gaussian) with CondLogLikelihoodLearner
  }

  def trainAndEvaluate(training: Seq[AbstractInstance], testing: Seq[AbstractInstance]) : Array[Array[Int]] = {
    val trainer = new RuntimeMaxEntTrainer(opts) { override val sGen = seqGen }
    val trainInsts = InstSeq(training)
    val m = trainer.batchTrainToModel(seqGen,trainInsts,getMe)
    val decoder = RuntimeMaxEntDecoder(m)
    val testInsts = InstSeq(testing)
    val pairs = testing map {inst => decoder.decodeInstance(inst); (inst.orig, inst.label)}
    val ns : Int = seqGen.getNumberOfStates
    getConfusionMatrix(ns, pairs)
  }

  def xValidate(n: Int) : IndexedSeq[Array[Array[Int]]] = {
    setInstances
    val instanceVec = instances.get.iseq
    val s = instanceVec.size
    val fSize = if ((s % n) == 0) s / n else (s / n) + 1
    val folds = instanceVec.sliding(fSize,fSize).toIndexedSeq
    for (i <- 0 until n) yield {
      val testFold = folds(i)
      val trainingFolds = for (j <- 0 until n if j != i) yield folds(j)
      trainAndEvaluate(trainingFolds.flatten, testFold)
    }
  }

  def getLabel(i: Int, invMap: scala.collection.mutable.Map[Int,AbstractLabel]) = {
    val l = invMap(i).toString
    if (l.size > 13) l.substring(0,13) else l
  }

  def colSum(m: Array[Array[Int]], i: Int) = {
    var sum = 0
    for (s <- 0 until m.size) sum += m(s)(i)
    sum
  }

  def rowSum(m: Array[Array[Int]], i: Int) = m(i).foldLeft(0){_ + _}

  def precision(m: Array[Array[Int]], i: Int) : Double = m(i)(i).toDouble / rowSum(m,i).toDouble
  def recall(m: Array[Array[Int]], i: Int) : Double = m(i)(i).toDouble / colSum(m,i).toDouble

  def accuracy(m: Array[Array[Int]]) : Double = {
    var diag = 0
    var tot = 0
    for (i <- 0 until m.size; j <- 0 until m.size) {if (i == j) diag += m(i)(i); tot += m(i)(j)}
    diag.toDouble / tot.toDouble
  }

  def reportOnMatrix(fname: String, os: java.io.Writer, mat: Array[Array[Int]]) = {
    os.write("\n\n================================================================================================================\n")
    os.write("fold: %s\n".format(fname))
    os.write("Accuracy: %f\n".format(accuracy(mat)))
    os.write("================================================================================================================\n")
    os.write("\t\thyp\\ref")
    val im = seqGen.invLa
    for (i <- 0 until mat.size) os.write("\t%15s".format(getLabel(i,im)))
    for (i <- 0 until mat.size) {
      os.write("\n\t%15s".format(getLabel(i,im)))
      for (j <- 0 until mat.size) os.write("\t%15d".format(mat(i)(j)))}
    os.write("\n\n")
    os.write("----------------------------------------------------------------------------------------------------------------\n")
    os.write("\t\t%35s\t%20s\t%20s".format("precision","recall","f-measure"))
    for (i <- 0 until mat.size) {
      val p = precision(mat,i)
      val r = recall(mat,i)
      val f = (2 * p * r) / (p + r)
      os.write("\n\t%15s\t%20f\t%20f\t%20f".format(getLabel(i,im),p,r,f))
    }
    os.write("\n\n")
  }

  def addTo(m1: Array[Array[Int]], m2: Array[Array[Int]]) = {
    for (i <- 0 until m1.size)
      for (j <- 0 until m2.size)
	m1(i)(j) += m2(i)(j)
  }
  

  def produceReport(nfolds: Int, f: java.io.File) = {
    val confMats = xValidate(nfolds)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(f)), "UTF-8")
    val lSize = seqGen.getNumberOfStates
    os.write("Cross Validation Report\n\n")
    os.write("Label categories: ")
    seqGen.getLAlphabet foreach {case (SLabel(l),_) => os.write(l + " ") case _ => }
    os.write("\n\n\nThere are " + nfolds.toString + " folds\n\n")
    val totalMat = Array.fill(lSize,lSize)(0)
    confMats.zipWithIndex foreach {case (mat,i) => reportOnMatrix(i.toString,os,mat); addTo(totalMat,mat)}
    os.write("\n\n Aggregate results over all folds:\n")
    reportOnMatrix("Aggregate",os,totalMat)
    os.close
  }

}

