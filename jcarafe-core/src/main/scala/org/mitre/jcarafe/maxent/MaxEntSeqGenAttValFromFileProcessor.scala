package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{ InstanceSequence, TextSeqGen, TrainingSeqGen, CrfInstance, MemoryInstanceSequence, SeqGen }
import org.mitre.jcarafe.util.SLabel
import java.io.File

trait MaxEntSeqGenAttValFromFileProcessor extends MaxEntSeqGen[List[(FeatureId, Double)]] {

  CrfInstance.training = true // need to set this so instances cache the right info
  
  //val sGen = new TrainingSeqGen[String](opts) with TextSeqGen
  val subSeqGen: SeqGen[String]

  def gatherFeatures(seqs: Seq[InstanceSequence]): Set[String] =
    seqs.foldLeft(Set(): Set[String]) { (cs, seq) => seq.iseq.foldLeft(cs) { (cs1, se) => se.userVec.foldLeft(cs1) { _ + _.getName } } }

  def mapToMaxEntInstance(lab: String, fs: Seq[InstanceSequence], fileName: String) = {
    val meFs: Set[String] = gatherFeatures(fs)
    val src = createSource(SLabel(lab), meFs.toList map { fn => (new FeatureId(fn), 1.0) })
    val inst = frep.createMEInstance(src.label, src.label, fileName)
    //frep.addMEFeature(inst, unkCode, 1.0)
    //src.obs foreach {case (l,v) => frep.addMEFeature(inst,l.fnId,v)}
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
