/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.semisupervised
import collection.mutable.HashMap
import org.mitre.jcarafe.crf.{ StdDecoder, DecodingAlgorithm, DenseCrf, AccessSeq, AbstractInstance, Crf, FeatureType, InducedFeatureMapProtocol, CoreModel }
import org.mitre.jcarafe.util.Options
import org.mitre.jcarafe.util.FastLoops._
import sbinary._

/*
 */

abstract class PosteriorDecoder(opts: Options, m: String) extends StdDecoder(opts, m) {

  val crfModel = model.crf
  val crf = {
    val c = new PosteriorCrf(crfModel.nls, crfModel.nfs, model.segSize)
    Array.copy(crfModel.params, 0, c.lambdas, 0, crfModel.nfs) // copy in existing model params into new crf model
    c
  }

  def getFeatureMap = crf.faWeightMap

  def runPosteriorDecoder(dobj: sGen.DeserializationT, decoder: DecodingAlgorithm): Unit = {
    val srcs = sGen.toSources(dobj)
    srcs foreach { srcS =>
      val srcSInst = sGen.extractFeatures(srcS)
      crf.marginalsOfSeq(srcSInst.iseq)
    }
  }

  def normalizeExpectations() = {
    val fsetmap = model.fsetMap
    println("\nnumber of self induced features: " + crf.faWeightMap.size)
    crf.faWeightMap foreach {
      case (fname, farray) =>
        val sum = farray.foldLeft(0.0)(_ + _)
        if (sum > 0.0) forIndex(crf.nls) { i => farray(i) = farray(i) / sum }
    }
    opts.weightedFeatureMap match {
      case Some(mapFile) =>
        InducedFeatureMapProtocol.writeFMap(crf.faWeightMap,new java.io.File(mapFile))
      case None => throw new RuntimeException("Expected map file to store induced feature/label weights")
    }
  }

  override def decodeSeqsFromFiles(decoder: DecodingAlgorithm): Unit = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new java.io.File(dirStr)
        val fs =
          dir.listFiles filter
            { f: java.io.File =>
              if (!f.isFile) false
              else pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } }
        val osuffix = opts.outSuffix match { case Some(o) => o case None => "" }
        fs foreach { f =>
          val deser = sGen.deserializeFromFile(f)
          runPosteriorDecoder(deser, decoder)
          print(".")
        }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            val deser = sGen.deserializeFromFile(f)
            runPosteriorDecoder(deser, decoder)
          case None =>
            throw new RuntimeException("Expecting input file or input directory")
        }
    }
    normalizeExpectations()
  }
}

class PosteriorCrf(nls: Int, nfs: Int, segSize: Int) extends DenseCrf(nls, nfs, segSize, 1.0) {

  var alphas: Matrix = Array.fill(1, nls)(0.0) // need to keep alphas to compute marginals

  def train(accessSeq: AccessSeq[AbstractInstance], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None) = throw new RuntimeException("Training not supported with posterior CRF")

  val faWeightMap = new HashMap[Long, Array[Double]]

  override protected def reset(all: Boolean, slen: Int): Unit = {
    super.reset(all, slen)
    val aNls = if (adjustible) slen else nls
    if (alphas.length < slen) {
      alphas = Array.fill(2 * slen, aNls)(0.0)
    }
  }

  override protected def forwardPass(iseq: Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScores(instFeatures, true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      Array.copy(newA, 0, curA, 0, curNls)
      assign(curA, (_ / scale(i)))
      Array.copy(curA, 0, alphas(i), 0, curNls)
      i += 1
    }
    seqLogLi
  }

  def marginalsOfSeq(iseq: Seq[AbstractInstance]): Unit = {
    val ilen = iseq.length
    if (ilen > 0) {
      reset(false, ilen)
      var xx = 0
      while (xx < nfs) { featureExpectations(xx) = 0.0; xx += 1 }
      backwardPass(iseq)
      var sll = forwardPass(iseq)
      updateFeatureMasses(iseq)
    }
  }

  private def updateFeatureMasses(iseq: Seq[AbstractInstance]) = {
    forIndex(iseq.length) { i =>
      val inst = iseq(i)
      val selfFeatures = inst.selfUserVec
      var zx_i = 0.0 // because of scaling, we need to compute normalization factor for this position in sequence
      forIndex(nls) { p => zx_i += alphas(i)(p) * beta(i)(p) }
      selfFeatures foreach { fname =>
        val farray = faWeightMap.get(fname) match {
          case Some(a) => a
          case None =>
            val a = Array.fill(nls)(0.0)
            faWeightMap.update(fname, a)
            a
        }
        forIndex(nls) { ycur =>
          farray(ycur) += (alphas(i)(ycur) * beta(i)(ycur) / zx_i)
        }
      }
    }
  }
}

