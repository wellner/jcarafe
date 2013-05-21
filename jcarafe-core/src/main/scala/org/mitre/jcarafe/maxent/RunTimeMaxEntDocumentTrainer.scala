package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{InstanceSequence, TrainingSeqGen, TextSeqGen, MemoryInstanceSequence, CoreModel, Trainable, MaxEntModel}
import collection.mutable.ListBuffer

class RunTimeMaxEntDocumentTrainer extends MaxEntTrainer(RunTimeOptions) {
  
  import scala.collection.JavaConverters._

  override val sGen = new FileBasedMaxEntTrainingSeqGen(RunTimeOptions)
  
  val subSeqGen = new TrainingSeqGen[String](RunTimeOptions) with TextSeqGen
  
  val instBuffer = new ListBuffer[MaxEntInstance]
  
  lazy val evaluator = getEvaluator 
  
  def getEvaluator = {
    val accessSeq = new MaxEntMemoryAccessSeq(Seq(new MemoryInstanceSequence(instBuffer.toSeq)))
    val evaluator = new Evaluator(opts, sGen)
    evaluator.addInstances(accessSeq.getSeqs(0))
    evaluator
  }
  
  private def documentStringToInstance(str: String, label: String) : MaxEntInstance = {
    sGen.mapToMaxEntInstance(label, subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromString(str)), "")
  }
  
  def getNumberOfInstances = instBuffer.size
  
  def addDocumentAsTrainingInstance(str: String, label: String) : Unit = {
    instBuffer append documentStringToInstance(str,label)
  }
  
  private def fileToString(f: java.io.File) = {
    val src = io.Source.fromFile(f)
    val buf = new StringBuilder
    src.getLines foreach {l => buf append l; buf append ' ';}
    buf.toString()
  }
  
  def addDocumentFileAsTrainingInstance(file: java.io.File, label: String) : Unit = {
    val str = fileToString(file)
    addDocumentAsTrainingInstance(str,label)
  }
  
  def xvalidate : Double = {
    val confMatrix = evaluator.xValidate(5) map {_._1}
    val nls = confMatrix(0).length
    val totalMat = Array.fill(nls,nls)(0)
    confMatrix foreach {mat => evaluator.addTo(totalMat,mat)}
    evaluator.accuracy(totalMat)
  }
  
  def trainModelToDecoder = {
    val me = getMeEstimator
    val accessSeq = new MaxEntMemoryAccessSeq(Seq(new MemoryInstanceSequence(instBuffer.toSeq)))
    val coreModel = me.train(accessSeq, opts.maxIters)
    val m = new MaxEntModel(sGen.getLAlphabet, coreModel, sGen.frep.fMap, sGen.getInducedFeatureMap)
    RunTimeMaxEntDocumentDecoder(m)
  }

}

