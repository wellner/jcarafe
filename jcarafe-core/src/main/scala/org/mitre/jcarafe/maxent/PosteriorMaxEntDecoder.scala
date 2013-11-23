/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.util.{ Options, SLabel }
import org.mitre.jcarafe.crf.{ MaxEntModel, DecodingAlgorithm, AbstractInstance, InducedFeatureMap, SeqGenScorer, InstanceSequence, CrfInstance,
  TrainingSeqGen, TextSeqGen}
import collection.mutable.HashMap
import scala.math._

class PosteriorMaxEntDecoder(decodingOpts: MEOptions, model: MaxEntModel) extends MaxEntDecoder(decodingOpts, model) with MaxEntCore {

  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  import org.mitre.jcarafe.util.FastLoops._

  val crfModel = model.crf

  val faWeightMap = new HashMap[Long, Array[Double]]

  override val sGen = new MaxEntDecodeSeqGen(model, decodingOpts) with SeqGenScorer[List[(FeatureId, Double)]] {
    override val frep = new SelfInducibleMEFRep[List[(FeatureId, Double)]](Some(model))
  }

  def runPosteriorDecoder(deser: sGen.DeserializationT, decoder: DecodingAlgorithm) = {
    val instr = deser.is
    var l = instr.readLine()
    while (l != null) { // ick, Java
      if (l.length > 2) { // just skip short/empty lines
        l.split(" ").toList match {
          case lab :: fs =>
            val src = sGen.createSource(SLabel(lab), (fs map { el =>
              el.split(":").toList match {
                case a :: b :: Nil => (FeatureId(a), b.toDouble)
                case a :: _ => (FeatureId(a), 1.0)
                case Nil => throw new RuntimeException("Feature vector parse failed")
              }
            }), false)
            val inst = sGen.frep.createMEInstance(src.label, src.label)
            sGen.addInFeatures(inst, src)
            val scores = classScoresNormalized(crfModel.nls, crfModel.nfs / crfModel.nls, crfModel.params, inst.getCompactVec)
            updateFeatureMasses(scores, inst)
          case _ =>
        }
      }
      l = instr.readLine()
    }
  }
  

  def runPosteriorDecoderFromFiles(decoder: DecodingAlgorithm) = {
    CrfInstance.training = true // need to set this so instances cache the right info
    decodingOpts.inputDir match {
      case Some(dirStr) =>
        val dir = new java.io.File(dirStr)
        dir.listFiles.toSeq foreach {f : java.io.File =>
          val inst = mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromFile(f)))
          val scores = classScoresNormalized(crfModel.nls, crfModel.nfs / crfModel.nls, crfModel.params, inst.getCompactVec)
          updateFeatureMasses(scores, inst)
        }
      case None => throw new RuntimeException("Must specify an input DIRECTORY with file-processing mode")
    }
  }

  override def runDecoder(deser: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]) =
    runPosteriorDecoder(deser, decoder)

  def normalizeExpectations() = {
    val fsetmap = model.fsetMap
    faWeightMap foreach {
      case (fname, farray) =>
        val sum = farray.foldLeft(0.0)(_ + _)
        if (sum > 0.0) forIndex(crfModel.nls) { i => farray(i) = farray(i) / sum }
    }
  }
  
  override def decodeFileBased() = {
    val decoder = new MaxEntDecodingAlgorithm(model.crf)
    runPosteriorDecoderFromFiles(decoder)
  }

  override def decode() = {
    if (decodingOpts.fileBased) decodeFileBased() else super.decode()
    normalizeExpectations()
  }

  def updateFeatureMasses(scores: Seq[Double], inst: AbstractInstance) = {
    val selfFeatures = inst.selfUserVec
    inst.selfUserVec foreach { fname =>
      val farray = faWeightMap.get(fname) match {
        case Some(a) => a
        case None =>
          val a = Array.fill(crfModel.nls)(0.0)
          faWeightMap.update(fname, a)
          a
      }
      forIndex(crfModel.nls) { i => farray(i) += scores(i) }
    }
  }
}

class SelfInducibleMaxEntInstance(l: Int, o: Int) extends MaxEntInstance(l, o) {
  var selfUserFeatures: Set[Long] = Set()
  override def addSelf(l: Long): Unit = selfUserFeatures += l
  override def selfUserVec: Set[Long] = selfUserFeatures
}

class SelfInducibleMEFRep[Obs](m: Option[MaxEntModel]) extends MEFRep[Obs](m) {

  override def createMEInstance(l: Int, o: Int) = new SelfInducibleMaxEntInstance(l, o)
  override def getInducedFeatureMap: Option[InducedFeatureMap] = m.get.inducedMap

  override def addMEFeature(inst: MaxEntInstance, fname: Long, vl: Double, clWts: Option[Array[Double]]): Unit = {
    super.addMEFeature(inst, fname, vl, clWts)
    inst.addSelf(fname) // add this feature (regardless of whether we saw it during training) to self-inducible feature set
  }

  def addMEFeatureNoSelf(inst: MaxEntInstance, fname: Long, vl: Double): Unit = {
    super.addMEFeature(inst, fname, vl)
  }
}
