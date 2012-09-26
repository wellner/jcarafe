package org.mitre.jcarafe.dparser

import collection.mutable.ListBuffer
import org.mitre.jcarafe.crf.{SeqGen, SourceSequence, ObsSource, InstanceSequence, Deserialization}
import org.mitre.jcarafe.util.AbstractLabel

class CoNLLDeserialization(val elements: Seq[Seq[List[String]]]) extends Deserialization {
  type T = Seq[Seq[List[String]]]
  lazy val indexed = elements.toIndexedSeq
  def getSlice(s: Int, e: Int) = new CoNLLDeserialization(indexed.slice(s, e).toList)
}

trait CoNLLSeqGen extends SeqGen[String] {

  type DeserializationT = CoNLLDeserialization
  
  def createSource(l:Int,o:String, i: Map[String,String]) = {
    frep.createSource(l,o,true,Some(i))
  }
  
  def deserializeFromFile(s: String): DeserializationT = {
    val src = scala.io.Source.fromFile(new java.io.File(s))
    var lines = src.getLines()
    var counter = 0
    val lSets = new ListBuffer[Seq[List[String]]]
    val curS = new ListBuffer[List[String]]
    val splitChar = if (opts.useSpaces) ' ' else '\t'
    lines foreach { l =>
      if ((l.length < 1) || (l == "++")) { // sequence boundary
        if (curS.length > 0)
          lSets += curS.toSeq
        curS.clear
      } else {
        curS += l.split(splitChar).toList
      }
    }
    if (curS.size > 0) lSets += curS.toSeq
    new CoNLLDeserialization(lSets.toSeq)
  }

  def deserializeFromString(s: String): DeserializationT = throw new RuntimeException("Unwritten method")
  def deserializeFromRawString(s: String): DeserializationT = deserializeFromString(s)
  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT = throw new RuntimeException("Unwritten method")
  
  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]) : String = 
    throw new RuntimeException("Unsupported method: seqsToString")	 

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = 
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Xml mode")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[InstanceSequence]) : scala.collection.immutable.Map[AbstractLabel,ListBuffer[org.mitre.jcarafe.util.Annotation]] = 
    throw new RuntimeException("Unsupported method: seqsToAnnotations")
  
  def toSources(d: DeserializationT): Seqs = {
    var sourceBuffer: ListBuffer[SourceSequence[String]] = new ListBuffer
    val splitChar = if (opts.useSpaces) ' ' else '\t'
    d.elements.foreach { seq =>
      val tmpBuf = new ListBuffer[ObsSource[String]]
      seq foreach { line =>
        line match {
          case id :: string :: _ :: pos :: _ :: _ :: gov :: label :: _ =>
            val ii = id.toInt
            val gg = gov.toInt
            val rGov = if (gg == 0) ii - 1 else gg - 1
            tmpBuf += createSource(rGov, string, Map("pos" -> pos))
          case _ =>
        }
      }
      if (tmpBuf.size > 0) {
        val seq = tmpBuf.toIndexedSeq
        sourceBuffer += new SourceSequence(seq)
      }
    }
    sourceBuffer.toSeq
  }

  def initialize() : Unit = throw new RuntimeException("Unsupported method: initialize")

  def seqsToAttributedDeserialization(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: seqsToAttributedDeserialization")
  
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = {
    var i = 0
    val nd = 
      d.elements map { seq => 
        var seqIndex = 0
        val iseq = seqs(i).iseq
        i += 1
        seq map { line => 
          line match {
            case id :: string :: x1 :: pos :: x2 :: x3 :: gov :: label :: r =>
              val g = iseq(seqIndex).label
              val i = id.toInt
              val gg = if (g == (i-1)) 0 else (g+1)
              seqIndex += 1
              id :: string :: x1 :: pos :: x2 :: x3 :: gg.toString :: label :: r
            case r => r
          }
        }
      }
    new CoNLLDeserialization(nd)
  }
  
  def seqsToStream(dt: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream): Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val outD = seqsToDeserialized(dt,seqs)
    outD.elements foreach { seq =>
      seq foreach { line =>
        var beg = true
        line foreach {el => 
          if (!beg) os.write('\t') 
          os.write(el)
          beg = false}
        os.write('\n')
      }
      os.write('\n')
    }
    os.close
    ostr.close
  }
  
  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File): Unit =
    seqsToStream(d, seqs, new java.io.FileOutputStream(f))

    
}