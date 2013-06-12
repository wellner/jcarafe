package org.mitre.jcarafe.crf

import scala.collection.mutable.ListBuffer

import org.mitre.jcarafe.util._

class BasicSeqDeserialization(val elements: Seq[Seq[String]]) extends Deserialization {
  type T = Seq[Seq[String]]
  lazy val indexed = elements.toIndexedSeq
  def getSlice(s: Int, e: Int) = new BasicSeqDeserialization(indexed.slice(s, e).toList)
}

trait BasicSeqGen extends SeqGen[String] with FactoredSeqGen[String] with XmlConversions {

  type DeserializationT = BasicSeqDeserialization
  val utf8_codec = scala.io.Codec("utf-8")
  
  private val LabRe = """([A-z]+)=([0-9\.]+)""".r

  private def getLabelDistribution(lst: Array[String]) = {
    val buf = new collection.mutable.ListBuffer[(String, Double)]
    var i = 0;
    var c = true
    while (i < lst.length && c) {
      if (LabRe.findFirstIn(lst(i)).isDefined) {
        lst(i) match {
          case LabRe(l, v) => buf append ((l, v.toDouble))
        }
      } else c = false
      i += 1
    }
    buf.toList
  }


  def deserializeFromFile(file: String): DeserializationT = {
    val src = scala.io.Source.fromFile(new java.io.File(file))
    var lines = src.getLines()
    var counter = 0
    val lSets = new ListBuffer[Seq[String]]
    val curL = new ListBuffer[String]
    lines foreach { l =>
      if ((l.length < 1) || (l == "++")) {
        if (curL.length > 0)
          lSets += curL.toSeq
        curL.clear
      } else {
        curL += l
      }
    }
    if (curL.size > 0) lSets += curL.toSeq
    new BasicSeqDeserialization(lSets.toSeq)
  }

  def deserializeFromString(s: String): DeserializationT = throw new RuntimeException("Unwritten method")
  def deserializeFromRawString(s: String): DeserializationT = deserializeFromString(s)
  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT = throw new RuntimeException("Unwritten method")
  
  
  def toSources(d: DeserializationT): Seqs = {
    var sourceBuffer: ListBuffer[SourceSequence[String]] = new ListBuffer
    var curAtts: Map[String, String] = Map.empty
    var curObs = ""
    var curLabel = ""
    val splitChar = if (opts.useSpaces) ' ' else '\t'
    d.elements.foreach { seq =>
      val tmpBuf = new ListBuffer[ObsSource[String]]
      seq foreach { line =>
        val chunks = line.split(splitChar).toArray
        var counter = 0
        var commenting = false
        val labelDist = getLabelDistribution(chunks)
        val nEls = chunks.length
        if (labelDist.length > 1) {
          counter = labelDist.length          
        }
        while ((counter < nEls) && !commenting) {
          if (counter == 0) {
            curLabel = chunks(0)
          } else {
            var vals = chunks(counter).split(':').toList
            vals match {
              case "#" :: _ => commenting = true
              case f :: v :: Nil => if (!commenting) curAtts += f -> v
              case f :: Nil => if (!commenting) curAtts += f -> "1.0"
              case _ => throw new RuntimeException("Line parsing failed: " + line)
            }
          }
          counter += 1
        }
        if (chunks.length > 0) {
          if (labelDist.length > 1) 
            tmpBuf += createDistributionalSource((labelDist map {case (l,s) => (SLabel(l),s)}),"",true,curAtts)
          else tmpBuf += createSource(new SLabel(curLabel), "", true, curAtts)
        }
        curAtts = Map() // reset map
      }
      if (tmpBuf.size > 0) {
        val seq = tmpBuf.toIndexedSeq
        sourceBuffer += new SourceSequence(seq)
      }
    }
    sourceBuffer.toSeq
  }

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

  val nlSep = System.getProperty("line.separator")

  def seqsToStream(dt: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream): Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val d = dt.elements.toIndexedSeq
    var curSeqI = 0
    seqs foreach { seq =>
      val dseq = d(curSeqI)
      var c = 0
      seq.iseq.foreach { sourceObj =>
        val curLine = dseq(c)
        os.write(invLa(sourceObj.label).toString)
        os.write('\t')
        os.write(curLine)
        c += 1
        os.write(nlSep)
      }
      os.write(nlSep)
      curSeqI += 1
    }
    os.flush
    os.close
  }
}
