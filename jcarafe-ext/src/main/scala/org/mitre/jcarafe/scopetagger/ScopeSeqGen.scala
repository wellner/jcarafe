package org.mitre.jcarafe.scopetagger

import org.mitre.jcarafe.crf.TextSeqGen
import org.mitre.jcarafe.crf.JsonSeqGen
import org.mitre.jcarafe.crf.SourceSequence
import org.mitre.jcarafe.crf.ObsSource
import org.mitre.jcarafe.tokenizer.WhiteSpaceTokenizer

class PostSourceSequence(seq: Seq[ObsSource[String]], parentSeq: Option[SourceSequence[String]], st: Int, en: Int) 
extends SourceSequence[String](seq, parentSeq, st, en) {
  def this(s: Seq[ObsSource[String]]) = this(s,None, -1, -1)
  def this(s: Seq[ObsSource[String]], st: Int, en: Int) = this(s, None, st, en)
  val info = new scala.collection.mutable.HashMap[String,String]
  for (j <- 0 until seq.length) {
    val ob = seq(j)
    ob.info match {case Some(i) => i.get("type") match {case Some(t) =>
      info += ("cueType" -> t); info += ("cueStartPos" -> j.toString) case None => } case None => }
  }
  override def getInfo(k: String) = { info.get(k) }
}

trait ScopeSeqGen extends TextSeqGen {
  override def createSourceSequence(ss: Seq[ObsSource[String]], st: Int, en: Int) : SourceSequence[String] = new PostSourceSequence(ss, st, en)
  override def createSourceSequence(ss: Seq[ObsSource[String]]) = new PostSourceSequence(ss)
}

trait ScopeJsonSeqGen extends JsonSeqGen {
  override val asPreProc = true
  override def createSourceSequence(ss: Seq[ObsSource[String]], st: Int, en: Int) : SourceSequence[String] = new PostSourceSequence(ss, st, en)
  override def createSourceSequence(ss: Seq[ObsSource[String]]) = new PostSourceSequence(ss)
  override def tokenizeSignal(signal: String) = WhiteSpaceTokenizer.parseString(signal)
}
