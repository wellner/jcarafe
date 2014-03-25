package org.mitre.jcarafe.crf

import scala.collection.mutable.ListBuffer

import org.mitre.jcarafe.util._

class DirectSeqDeserialization(val elements: Seq[Seq[(String,Array[String])]]) extends Deserialization {
  type T = Seq[Seq[(String, Array[String])]]
  lazy val indexed = elements.toIndexedSeq
  def getSlice(s: Int, e: Int) = new DirectSeqDeserialization(indexed.slice(s, e).toList)
}

object DirectSeqDeserialization {
  def buildFromUnlabeledInstances(els: Seq[Seq[Array[String]]]) = new DirectSeqDeserialization(els map {els1 => els1 map {ar => ("",ar)}})
}


trait DirectSeqGen extends SeqGen[String] with FactoredSeqGen[String] {

  type DeserializationT = DirectSeqDeserialization
  
  def deserializeFromFile(file: String): DeserializationT = {
    val src = scala.io.Source.fromFile(new java.io.File(file))
    //var curAtts: Map[String, String] = Map.empty
    var curAtts : ListBuffer[String] = new ListBuffer[String]
    var curObs = ""
    var curLabel = ""
    var lines = src.getLines()
    var counter = 0
    val seqs = new ListBuffer[Seq[(String, Array[String])]]
    val splitChar = if (opts.useSpaces) ' ' else '\t'
    var seq = new ListBuffer[(String, Array[String])]
    lines foreach { l =>
      if ((l.length < 1) || (l == "++")) {
        seqs append seq.toSeq
        seq.clear
      } 
      else {
        val chunks = l.split(splitChar).toIndexedSeq
        var counter = 0
        var commenting = false
        while (counter < chunks.length) {
          if (counter == 0) {
            curLabel = chunks(0)
          } else {
            var vals = chunks(counter).split(':').toList
            vals match {
              case "#" :: _ => commenting = true
              case f :: v :: Nil => if (!commenting) curAtts += f
              case f :: Nil => if (!commenting) curAtts += f
              case _ => throw new RuntimeException("Line parsing failed: " + l)
            }
          }
          counter += 1
        }
        if (chunks.length > 0) {
          seq append ((curLabel, curAtts.toArray)) 
        }
        curAtts.clear() // reset map
      }
    }
    if (!seq.isEmpty) seqs append seq.toSeq // add in last sequence if it's not empty 
    new DirectSeqDeserialization(seqs.toSeq)
  }
  
  def toSources(d: DeserializationT): Seqs = {
    var sourceBuffer: ListBuffer[SourceSequence[String]] = new ListBuffer
     
    d.elements foreach { seq =>
      val tmpBuf = new ListBuffer[ObsSource[String]]
      seq foreach { case (lab, atts) =>
        var curAtts = Map[String,String]()
        atts foreach {att =>
          curAtts += ((att,"1.0"))
          }
        tmpBuf += createSource(new SLabel(lab), "", true, curAtts)
      }
      sourceBuffer append (new SourceSequence(tmpBuf.toIndexedSeq))
    }
    sourceBuffer.toVector
  }
  val nlSep = System.getProperty("line.separator")
  
  def seqsToStream(dt: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream): Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val d = dt.elements.toIndexedSeq
    var curSeqI = 0
    seqs foreach { seq =>
      val dseq = d(curSeqI)
      var c = 0
      seq.iseq.foreach { sourceObj =>
        val (label,atts) = dseq(c)
        os.write(invLa(sourceObj.label).toString)
        os.write('\t')
        atts foreach {a => os.write(a); os.write('\t')}
        c += 1
        os.write(nlSep)
      }
      os.write(nlSep)
      curSeqI += 1
    }
    os.flush
    os.close
  }
  
  def deserializeFromString(s: String): DeserializationT = throw new RuntimeException("Unwritten method")
  def deserializeFromRawString(s: String): DeserializationT = deserializeFromString(s)
  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT = throw new RuntimeException("Unwritten method")
  
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT =
    throw new RuntimeException("This method unsupported for Text mode currently")

  def seqsToAnnotations(d: DeserializationT, seqs: Seq[InstanceSequence]): Map[AbstractLabel, ListBuffer[Annotation]] =
    throw new RuntimeException("Unsupported method: seqsToAnnotations")

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter, close: Boolean = true): Unit =
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Basic mode")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File): Unit =
    seqsToStream(d, seqs, new java.io.FileOutputStream(f))

  def initialize(): Unit =
    if (addBeginStates) StateCache.updateStateCache(lAlphabet) // make sure the state cache is updated for handling BEGIN states

  def seqsToString(dt: DeserializationT, seqs: Seq[InstanceSequence]): String = {
    throw new RuntimeException("Unwritten method")
  }

}