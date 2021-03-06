/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.DecodingAlgorithm
import scala.math._
import scala.util.Random
import org.mitre.jcarafe.crf.ObsSource
import org.mitre.jcarafe.crf.StdModel
import org.mitre.jcarafe.crf.Alphabet
import org.mitre.jcarafe.crf.{ InstanceSequence, AbstractInstance, SeqGen, PsaLearner, CondLogLikelihoodLearner, InstSeq, SourceSequence }
import org.mitre.jcarafe.util._
import org.mitre.jcarafe.util.SLabel
import org.mitre.jcarafe.crf.IncrementalMurmurHash

class Evaluator(val opts: MEOptions, val seqGen: MaxEntTrainingSeqGen, val decodingSeqGen: Option[MaxEntSeqGenAttVal] = None) {
  def this(opts: MEOptions, sg: MaxEntSeqGenAttVal) = this(opts, new MaxEntTrainingSeqGen(new MEOptions()), Some(sg))
  def this(opts: MEOptions) = this(opts, new MaxEntTrainingSeqGen(new MEOptions()))  
  def this() = this(new MEOptions(), new MaxEntTrainingSeqGen(new MEOptions()))
  
  import scala.collection.JavaConversions._

  import IncrementalMurmurHash._

  var instances: Option[InstanceSequence] = None
  
  private var sources: Option[seqGen.Seqs] = None
  
  def setSources = {
    sources = Some(seqGen.createSourcesFromFiles(0))
  }

  val decodedOutputStream = opts.outputFile map { f => new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(f)), "UTF-8") }

  def setInstances: Unit = { instances = Some(InstSeq(Random.shuffle(instances.get.iseq))) }

  def addInstances(i: InstanceSequence) = { instances = Some(i) }
  
  def addJInstance(l: String, fs: java.util.List[String]): Unit = throw new RuntimeException("Not yet implemented")

  def getConfusionMatrix(s: Int, pairs: Seq[(Int, Int)]): Array[Array[Int]] = {
    val matrix = Array.fill(s, s)(0)
    pairs foreach { case (a, b) => matrix(a)(b) += 1 }
    matrix
  }

  private def getMe = {
    if (opts.parallel) {
      val numPs = opts.numThreads match {
        case None => Runtime.getRuntime.availableProcessors * 4 / 5 // leave a CPU or two free
        case Some(n) => n
      }
      println(">> Initiating Parallel Training using " + numPs + " processors <<\n")
      new DenseParallelMaxEnt(numPs, seqGen.getNumberOfStates, seqGen.getNumberOfFeatures, opts.gaussian)
    } else if (opts.psa) {
      new SparseMaxEnt(seqGen.getNumberOfStates, seqGen.getNumberOfFeatures, opts) with PsaLearner[AbstractInstance]
    } else new MaxEnt(seqGen.getNumberOfStates, seqGen.getNumberOfFeatures, opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
  }

  /*
  def trainAndEvaluate(training: Seq[AbstractInstance], testing: Seq[AbstractInstance]): (Array[Array[Int]], Double) = {
    val trainer = new RuntimeMaxEntTrainer(opts) { override val sGen = seqGen }
    val trainInsts = InstSeq(training)
    val m = trainer.batchTrainToModel(seqGen, trainInsts, getMe)
    val decoder = RuntimeMaxEntDecoder(m)    
    val testInsts = InstSeq(testing)
    evaluate(decoder, Seq(testInsts))
  }
  * 
  */
  
  def trainAndEvaluate(training: Seq[ObsSource[List[(FeatureId,Double)]]], testing: Seq[ObsSource[List[(FeatureId,Double)]]]) : (Array[Array[Int]], Double) = {
    println("There are: " + training.length + " training instances and " + testing.length + " testing instances")
    val trainer = new RuntimeMaxEntTrainer(opts)
    seqGen.getLAlphabet.mp foreach { case (k,v) => trainer.sGen.lAlphabet update (k,v)} // set label alphabet
    val trInsts = trainer.sGen.extractFeatures(new SourceSequence(training))
    println("Number of states: " + trainer.sGen.getNumberOfStates)
    trainer.sGen.frep.fMap.fixed_=(true) // fix the feature alphabet
    val tstInsts = trainer.sGen.extractFeatures(new SourceSequence(testing))
    val m = trainer.batchTrainToModel(trainer.sGen, trInsts)
    val decoder = RuntimeMaxEntDecoder(m)    
    evaluate(decoder, Seq(tstInsts))
  }
    
  def evaluate(decoder: RuntimeMaxEntDecoder, testInsts: Seq[InstanceSequence]) : (Array[Array[Int]], Double) = {
    val testing = testInsts.map{_.iseq}.foldLeft(Seq():Seq[AbstractInstance]){_ ++ _}
    var crossEnt = 0.0
    val pairs = testing map { inst =>
      val dist = decoder.decodeInstanceAsDistribution(inst)
      dist foreach {
        case (s, i) =>
          val empiricalProb_a = inst.conditionalProb(i)
          val empiricalProb = if (empiricalProb_a < 1E-200) 1E-200 else empiricalProb_a 
          val loss = empiricalProb * math.log(s)
          crossEnt -= loss
      }
      decoder.decodeInstance(inst)
      decodedOutputStream foreach {s =>
        s.write(inst.orig.toString + '\t' + inst.conditionalProb(inst.orig).toString)
        s.write('\n')
        }
      (inst.label, inst.orig)
    }
    val mat : Array[Array[Int]] = 
      if (opts.binomial) Array(Array())
      else {
        val ns: Int = decoder.sGen.getNumberOfStates
        getConfusionMatrix(ns, pairs)
      }
    (mat, crossEnt)
  }

  def xValidate(n: Int): IndexedSeq[(Array[Array[Int]], Double)] = {
    //setInstances
    //val instanceVec = instances.get.iseq
    //val s = instanceVec.size
    //val folds = instanceVec.sliding(fSize, fSize).toIndexedSeq
    val srcVec = sources.get(0).seq
    val s = srcVec.size
    val fSize = if ((s % n) == 0) s / n else (s / n) + 1
    val folds = srcVec.sliding(fSize,fSize).toIndexedSeq
    val r =
      for (i <- 0 until n) yield {
        val testFold = folds(i)
        val trainingFolds = for (j <- 0 until n if j != i) yield folds(j)
        trainAndEvaluate(trainingFolds.flatten, testFold)
      }
    decodedOutputStream foreach { _.close }
    r
  }
  
  def getLabel(i: Int, invMap: scala.collection.mutable.Map[Int, AbstractLabel]) = {
    val l = invMap(i).toString
    if (l.size > 13) l.substring(0, 13) else l
  }

  def colSum(m: Array[Array[Int]], i: Int) = {
    var sum = 0
    for (s <- 0 until m.size) sum += m(s)(i)
    sum
  }

  def rowSum(m: Array[Array[Int]], i: Int) = m(i).foldLeft(0) { _ + _ }

  def precision(m: Array[Array[Int]], i: Int): Double = m(i)(i).toDouble / rowSum(m, i).toDouble
  def recall(m: Array[Array[Int]], i: Int): Double = m(i)(i).toDouble / colSum(m, i).toDouble

  def accuracy(m: Array[Array[Int]]): Double = {
    var diag = 0
    var tot = 0
    for (i <- 0 until m.size; j <- 0 until m.size) { if (i == j) diag += m(i)(i); tot += m(i)(j) }
    diag.toDouble / tot.toDouble
  }

  def reportOnMatrix(xval: Boolean, fname: String, os: java.io.Writer, mat: Array[Array[Int]]) = {
    os.write("\n\n================================================================================================================\n")
    if (xval) os.write("fold: %s\n".format(fname))
    os.write("Accuracy: %f\n".format(accuracy(mat)))
    os.write("================================================================================================================\n")
    os.write("\t\thyp\\ref")
    val im = decodingSeqGen match {case Some(ds) => ds.invLa case None => seqGen.invLa}
    for (i <- 0 until mat.size) os.write("\t%15s".format(getLabel(i, im)))
    for (i <- 0 until mat.size) {
      os.write("\n\t%15s".format(getLabel(i, im)))
      for (j <- 0 until mat.size) os.write("\t%15d".format(mat(i)(j)))
    }
    os.write("\n\n")
    os.write("----------------------------------------------------------------------------------------------------------------\n")
    os.write("\t\t%35s\t%20s\t%20s".format("precision", "recall", "f-measure"))
    for (i <- 0 until mat.size) {
      val p = precision(mat, i)
      val r = recall(mat, i)
      val f = (2 * p * r) / (p + r)
      os.write("\n\t%15s\t%20f\t%20f\t%20f".format(getLabel(i, im), p, r, f))
    }
    os.write("\n\n")
  }

  def addTo(m1: Array[Array[Int]], m2: Array[Array[Int]]) = {
    for (i <- 0 until m1.size)
      for (j <- 0 until m2.size)
        m1(i)(j) += m2(i)(j)
  }
  
  def xValidateAndGenerateReport(nfolds: Int, f: java.io.File) = {
    val confMatsAndDivergence = xValidate(nfolds)
    produceReport(confMatsAndDivergence, nfolds, f)
  }

  def produceReport(confMatsAndDivergence: IndexedSeq[(Array[Array[Int]], Double)], nfolds: Int, f: java.io.File) = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(f)), "UTF-8")
    val lSize = seqGen.getNumberOfStates
    if (nfolds > 1) os.write("Cross Validation Report\n\n") else os.write("Evaluation Report\n\n")        
    if (opts.binomial) {
      val klScores = confMatsAndDivergence map { _._2 }
      var i = 0
      klScores foreach { s =>
        i += 1
        println("Cross-entropy score: " + s)
        os.write("Cross-entropy -- fold: " + i + " => " + s)        
        os.write('\n')
      }
      os.write("Cross-entropy Average: " + (klScores.foldLeft(0.0) { _ + _ } / klScores.length))
      os.write('\n')
    } else {
      os.write("Label categories: ")
      seqGen.getLAlphabet.mp foreach { case (SLabel(l), _) => os.write(l + " ") case _ => }
      if (nfolds > 1) os.write("\n\n\nThere are " + nfolds.toString + " folds\n\n")
      val totalMat = Array.fill(lSize, lSize)(0)
      val confMats = confMatsAndDivergence map { _._1 }

      confMats.zipWithIndex foreach { case (mat, i) => reportOnMatrix((nfolds > 1), i.toString, os, mat); addTo(totalMat, mat) }
      if (nfolds > 1) {
        os.write("\n\n Aggregate results over all folds:\n")
        reportOnMatrix(true, "Aggregate", os, totalMat)
      }
    }
    os.close
  }

}

