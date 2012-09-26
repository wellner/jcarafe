package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{MaxEntModel, MemoryInstanceSequence, MaxEntSerializer, SeqGenScorer}
import org.mitre.jcarafe.util.SLabel

object RunTimeOptions extends MEOptions { val fileProcessing = true; rawDecode = true}

class RunTimeMaxEntDocumentDecoder(m: MaxEntModel) extends MaxEntDecoder(RunTimeOptions, m) {
  
  import scala.collection.JavaConverters._

  val meDecoder = new MaxEntDecodingAlgorithm(model.crf)
  
  override val sGen = new FileBasedMaxEntDecodeSeqGen(model, RunTimeOptions) with SeqGenScorer[List[(FeatureId, Double)]] 
  
  def numLabels = meDecoder.numLabels
  
  private def getLabelString(i: Int) : String =
    sGen.invLa(i) match { case SLabel(l) => l case _ => throw new RuntimeException("Unexpected label type") }
  
  def decodeDocument(str: String) : String = {
    val inst = sGen.mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromString(str)), "")
    val labReturn = meDecoder.classifyInstance(inst)
    getLabelString(labReturn)
  }
  
  def decodeDocument(f: java.io.File) : String = {
    val inst = sGen.mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromFile(f)), "")
    val labReturn = meDecoder.classifyInstance(inst)
    getLabelString(labReturn)
  }
  
  def getDocumentPosterior(f: java.io.File) : java.util.List[(String,java.lang.Double)] = {
    val inst = sGen.mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromFile(f)), "")
    val rr : Seq[(String,java.lang.Double)] = meDecoder.getInstanceDistribution(inst).toSeq map {case (l,i) => (getLabelString(i),new java.lang.Double(l))}
    rr.asJava
  }
  
  def getDocumentPosterior(str: String) : java.util.List[(String,java.lang.Double)] = {
    val inst = sGen.mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromString(str)), "")
    val rr : Seq[(String,java.lang.Double)] = meDecoder.getInstanceDistribution(inst).toSeq map {case (l,i) => (getLabelString(i),new java.lang.Double(l))}
    rr.asJava
  }
  
  def serializeToFile(f: java.io.File) = RunTimeMaxEntDocumentDecoder.serializeModelToFile(this, f)
  
}

object RunTimeMaxEntDocumentDecoder {
  import MaxEntSerializer._
  def apply(m: String) = new RunTimeMaxEntDocumentDecoder(readModel(m.getBytes))
  def apply(m: java.io.File) = new RunTimeMaxEntDocumentDecoder(readModel(m))
  def apply(m: MaxEntModel) = new RunTimeMaxEntDocumentDecoder(m)
  def apply(m: java.io.InputStream) = new RunTimeMaxEntDocumentDecoder(readModel(m))
  
  def serializeModelToFile(decoder: RunTimeMaxEntDocumentDecoder, f: java.io.File) = {
    writeModel(decoder.model,f)
  }
}