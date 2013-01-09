package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{ InstanceSequence, TextSeqGen, TrainingSeqGen, CrfInstance, MemoryInstanceSequence, SeqGen }
import org.mitre.jcarafe.util.SLabel
import java.io.File

trait MaxEntSeqGenAttValFromFileProcessor extends MaxEntSeqGen[List[(FeatureId, Double)]] {

  CrfInstance.training = true // need to set this so instances cache the right info
  
  //val sGen = new TrainingSeqGen[String](opts) with TextSeqGen
  val subSeqGen: SeqGen[String]

  def gatherFeatures(seqs: Seq[InstanceSequence]): Map[String,Double] = {
    var fm : Map[String,Double] = Map()
    seqs foreach { seq => seq.iseq foreach { se => se.userVec foreach {ft => fm += (ft.getName -> ft.value)}}}
    fm
  }

  def mapToMaxEntInstance(lab: String, fs: Seq[InstanceSequence], fileName: String) = {
    val meFs: Map[String,Double] = gatherFeatures(fs)
    val src = createSource(SLabel(lab), meFs.toList map { case (fn,v) => (new FeatureId(fn), v) })
    val inst = frep.createMEInstance(src.label, src.label, fileName)
    addInFeatures(inst, src)
    inst
  }

  override def createSeqsFromFiles: Seq[InstanceSequence] = {
    
    opts.inputDir match {
      case Some(dirStr) =>
        val dir = new File(dirStr)
        val dirFiles = dir.listFiles
        val categoryDirs = dirFiles filter { _.isDirectory }
        if (categoryDirs.size == 0) {
          val ais = dirFiles map { f: File =>
            mapToMaxEntInstance("UNK", subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromFile(f)), f.getName)
          }
          Seq(new MemoryInstanceSequence(ais))
        } else {
          val ais = categoryDirs flatMap { catDir =>
            val dirName = catDir.getName
            catDir.listFiles.toSeq map { f: File => mapToMaxEntInstance(dirName, subSeqGen.createSeqsWithInput(subSeqGen.deserializeFromFile(f)), f.getName) }
          }
          Seq(new MemoryInstanceSequence(ais))
        }
      case None => throw new RuntimeException("Must specify an input DIRECTORY with file-processing mode")
    }
  }

}
