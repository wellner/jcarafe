/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.mitre.jcarafe.tokenizer._
import org.mitre.jcarafe.util._

class TextSeqDeserialization(val elements: List[Element]) extends Deserialization {
  type T = List[Element]

  lazy val indexed = elements.toIndexedSeq

  def getSlice(s: Int, e: Int) = new TextSeqDeserialization(indexed.slice(s, e).toList)
}

object TagParser extends RegexParsers {
  def tagExpr: Parser[AbstractLabel] = pref ~> elem ~ atts ~ suff ^^ { case (e ~ a ~ _) => Label(e, a) }
  def pref: Parser[String] = "</" | "<"
  def suff: Parser[String] = "/>" | ">"
  def elem: Parser[String] = "[^ \"\'<>]+".r
  def att: Parser[(String, String)] = "[^ \"\'<>=]+".r ~ "=" ~ "\"" ~ "[^\"<>]+".r ~ "\"" ^^ { case (a ~ _ ~ _ ~ v ~ _) => (a, v) }
  def atts: Parser[Map[String, String]] = rep(att) ^^ { l => l.foldLeft(Map(): Map[String, String]) { case (ac, kv) => ac + kv } }
  def parseIt(r: java.io.Reader): AbstractLabel = parse(tagExpr, new PagedSeqReader(PagedSeq.fromReader(r))).getOrElse(SLabel(""))
  def parseString(s: String): AbstractLabel = parseIt(new java.io.StringReader(s))
}

/**
 * Deserialization functionality for handling text input.  This uses a customized
 * lexer to identify tokens within a body of text, identifies sentence/zone boundaries
 * and produce annotations as inline tags.  It does not use an XML parser, however,
 * and will therefore produce and consume files that do not conform to XML.
 */
trait TextSeqGen extends SeqGen[String] with FactoredSeqGen[String] with XmlConversions {

  type DeserializationT = TextSeqDeserialization

  val MAX_SEQ_LEN = 4000

  var ignoreFlag = !boundaries.set.isEmpty // global state to indicate whether we're in an IGNORE block

  def deserializeFromFile(file: String): DeserializationT = 
    new TextSeqDeserialization(if (opts.rawDecode) FastTokenizer.parseFileNoTags(file) else FastTokenizer.parseFile(file,!opts.preProc))

  def deserializeFromString(s: String): DeserializationT = 
    new TextSeqDeserialization(if (opts.rawDecode) FastTokenizer.parseStringNoTags(s) else FastTokenizer.parseString(s,!opts.preProc))

  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT = new TextSeqDeserialization((seq map { el => Tok(el) }).toList)

  def deserializeFromRawString(s: String): DeserializationT = deserializeFromString(s)

  def getLabelAndAttrsFromTag(t: String): (String, Map[String, String]) = {
    TagParser.parseString(t) match {
      case Label(l, atts) => (l, atts)
      case _ => (t, Map()) // throw new RuntimeException("Unexpected/malformed label and attributes: " + t)
    }
  }

  lazy val lexAttributedTagSet = invLa.exists { case (i, v) => v match { case SLabel(_) => false case l => l.labelHead.equals("lex")} }

  private def appearsToBeEndOfSequence(els: List[Element]): Boolean = {
    els match {
      case Ws(w) :: _ => true
      case EndWs(w1) :: _ => true
      case Tag("lex", false) :: Ws(w) :: r => true
      case Tag("lex", false) :: EndWs(w1) :: r => true
      case _ => false
    }
  }

  def toSources(d: DeserializationT): Seqs = {
    val sourceBuffer: ListBuffer[SourceSequence[String]] = new ListBuffer
    var state = other
    var curAtts: Map[String, String] = Map.empty
    var tmpBuf = new ListBuffer[ObsSource[String]]
    var specialTok = false // flag whether we're within a special token
    val specialBuf = new StringBuilder
    var withinTag = false // simple check whether we're within a known tag
    var posWithinTag = 0 // check whether we're just inside a tag
    var endSeqOnNextToken = false
    var docPos = 0 // position within entire deserialization; used to split out tokens
    var prevPos = 0 // end of previous sequence
    def endSeq(t: Option[String], c: Boolean) = {
      t match { case Some(t) => tmpBuf += createSource(getAState(state, !c), t, !c, curAtts); curAtts = Map.empty case None => }
      if (tmpBuf.size > 0) {
        sourceBuffer += createSourceSequence(tmpBuf.toIndexedSeq, prevPos, docPos)
        tmpBuf = new ListBuffer[ObsSource[String]]
        prevPos = docPos
      }
    }
    def getAState(l: AbstractLabel, v: Boolean) = if (addBeginStates) getState(l, v) else l
    def gather(toks: List[Element], cont: Boolean): Unit = {
      docPos += 1
      if (specialTok) {
        toks match {
          case Tag("</lex>", _) :: r =>
            val beg = posWithinTag < 1
            val st = getAState(state, (posWithinTag < 1))
            posWithinTag += 1
            specialTok = false; tmpBuf += createSource(st, specialBuf.toString, beg, curAtts); curAtts = Map.empty; gather(r, true)
          case t :: r =>
            specialBuf.append(t.getString); gather(r, cont)
          case Nil => Nil
        }
      } else {
        toks match {
          case Tag(t, true) :: r =>
            if (endSeqOnNextToken) { endSeq(None, cont); endSeqOnNextToken = false }
            val (l, attmap) = getLabelAndAttrsFromTag(t) // parse tag to get label and attribute value pairs
            if (l == "lex") {
              specialBuf.clear
              curAtts = attmap
              specialTok = true
              if (!lexAttributedTagSet) withinTag = true
            }
            opts.tagset.getTag(l, attmap) match {
              case Some(lab) =>
                posWithinTag = 0
                if (l != "lex") withinTag = true // only used for multi-token tags
                state = lab; curAtts = attmap
              case None =>
                if (opts.partialLabels && (lexAttributedTagSet || (opts.uncertainTag match {case Some(utag) => utag equals l case None => false}))) {                  
                  state = new UncertainLabel
                  curAtts = attmap
                }
            }
            if (opts.boundaries.isWithin(l, attmap)) { endSeq(None, cont)}
            gather(r, false)
          case HardEndTok(t) :: r =>
            if (opts.preProc && !withinTag) endSeq(Some(t), cont)
            gather(r, cont)
          case EndWs(t) :: r => if (opts.preProc && !withinTag) endSeq(None, cont); gather(r, cont)
          case Ws(_) :: r => if (opts.preProc && !withinTag && ((docPos - prevPos) > MAX_SEQ_LEN)) endSeq(None, cont); gather(r, cont)
          case Tag(t, false) :: r => // note that closing lex tags won't reach this 
            val (l, atts) = getLabelAndAttrsFromTag(t)
            if (l != "lex") {
              if (opts.boundaries.labelMatch(l)) { endSeq(None, cont) }
              state = other
              withinTag = false
            }
            gather(r, false)
          case Tok(t) :: r =>
            // --- way to do this - subtract of 1 from document position and then add it back
            if (endSeqOnNextToken) { docPos -= 1; endSeq(None, cont); endSeqOnNextToken = false; docPos += 1 }
            if (opts.preProc) {
              posWithinTag += 1
              tmpBuf += createSource(getAState(state, !cont), t, !(cont), curAtts)
              curAtts = Map.empty
            }
            gather(r, true) // add to queue
          case SoftEndTok(t) :: r =>
            if (opts.preProc) {
              if (!withinTag && appearsToBeEndOfSequence(r)) {
                endSeqOnNextToken = true
              }
              posWithinTag += 1
              tmpBuf += createSource(getAState(state, !cont), t, !(cont), curAtts)
              curAtts = Map.empty
            }
            gather(r, true)
          case ComplexTok(t, _) :: r =>
            if (opts.preProc) {
              posWithinTag += 1
              tmpBuf += createSource(getAState(state, !cont), t, !(cont), curAtts)
              curAtts = Map.empty
            }
            gather(r, true) // add to queue
          case _ :: r => gather(r, cont)
          case Nil => sourceBuffer += createSourceSequence(tmpBuf.toIndexedSeq, prevPos, docPos) // add last buffer, even if its empty  
        }
      }
    }
    gather(d.elements, false)
    sourceBuffer.toIndexedSeq
  }

  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT =
    throw new RuntimeException("This method unsupported for Text mode currently")

  def seqsToAnnotations(d: DeserializationT, seqs: Seq[InstanceSequence]): Map[AbstractLabel, ListBuffer[Annotation]] =
    throw new RuntimeException("Unsupported method: seqsToAnnotations")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File): Unit =
    seqsToStream(d, seqs, new java.io.FileOutputStream(f))

  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]): String = {
    val os = new java.io.ByteArrayOutputStream()
    seqsToStream(d, seqs, os)
    os.toString("UTF-8")
  }
  
  def seqsToSlicedWriter(d: DeserializationT, dseq: SourceSequence[String], iseqs: InstanceSequence, os: java.io.OutputStreamWriter): Unit =
    seqsToWriter(d.getSlice(dseq.st, dseq.en), Seq(iseqs), os)

  def writeTok(write: Boolean, t: String, os: java.io.OutputStreamWriter, lexInfo: Option[Map[String,String]] = None): Unit = {
    if (opts.keepToks && write) {
      lexInfo match {
        case Some(i) => 
          os.write("<lex")
          i foreach {case (k,v) => os.write(' '); os.write(k); os.write("="); os.write('\"'); os.write(v); os.write('\"')}
          os.write('>')
        case None => os.write("<lex>")
      }
      os.write(t)
      os.write("</lex>")
    } else os.write(t)
  }

  def seqsToStream(d: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream, close: Boolean = true): Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    seqsToWriter(d, seqs, os)
    if (close) os.close
  }

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter, close: Boolean = true): Unit = {
    seqs foreach { seq =>
      val nd = d.getSlice(seq.st, seq.en)
      seqToWriter(nd, seq, os)
    }
    if (close) os.close
  }
  
  def seqToWriter(d: DeserializationT, iSeq: InstanceSequence, os: java.io.OutputStreamWriter): Unit = {
    if (addBeginStates) StateCache.updateStateCache(lAlphabet) // make sure the state cache is updated for handling BEGIN states
    var cs = 0
    var cp = 0
    val lexInd = lAlphabet.update(SLabel("lex"))
    var curLab = lexInd
    var specialTok = false
    val specialBuf = new StringBuilder
    var tokTagInfo : Option[Map[String,String]] = None
    val labSeq = iSeq.iseq
    var withinLex = false // that keeps track of whether we're within a lex-tag.  For no-pre-proc, we need to skip over everything outside a lex tag....
    def createNewTok(t: String, lexEnd: Boolean = false) = {
      // check whether this token should be included in the tagged output
      // if its outside the label sequence or we aren't doing pre-processing and it didn't appear
      // within the lex tag, we should skip it
      if ((cp < labSeq.length) && (opts.preProc || withinLex)) {
        val ilab = labSeq(cp).label
        val lab = invLa(ilab)
        val nlabState = lab match { case BeginState(l) => l case a => a }
        val normLab = if (addBeginStates) lAlphabet.update(nlabState) else ilab
        val isSpecialLex = { nlabState match { case Label(l, _) => l.equals("lex") case _ => false } } // always wrap each token in this case
        if ((((ilab != curLab) || (ilab != normLab)) && (ilab != lexInd)) || isSpecialLex) {
          val writeLex = !isSpecialLex 
          tokTagInfo match {
            case Some(m) =>
              if (!lexAttributedTagSet) {
                os.write(lab.labelTag(false,None))
              }
              else {
                os.write(lab.labelTag(false, Some(m), (opts.keepToks && lexAttributedTagSet)))
              }
            case None => // there isn't any special tok tag info
              os.write(lab.labelTag(false, None))
          } // print out lex tags here
          writeTok(writeLex, t, os, tokTagInfo)
        } else if (!isSpecialLex) {
          writeTok(true, t, os, tokTagInfo)
        } else writeTok(false, t, os, tokTagInfo)
        if (!isSpecialLex && lexEnd) { os.write("</lex>") }
        if (ilab != lexInd) {
          if (cp < labSeq.length - 1) {
            val nlab = labSeq(cp + 1).label
            if (((nlab != ilab) && (nlab != normLab)) || StateCache.isBegin(nlab) || isSpecialLex) os.write(lab.labelTag(true, None))
          } else os.write(lab.labelTag(true, None))
        }
        cp += 1
        curLab = normLab
      } else {
        // this case will/should only happen when the tokens were not captured as part of a sequence
        // we should just print them back out
        os.write(t)
      }
      tokTagInfo = None // reset tag info 
    }

    def traverse(toks: List[Element]): Unit = {
      if (specialTok) {
        toks match {
          case Tag("</lex>", _) :: r => 
            specialTok = false; createNewTok(specialBuf.toString, lexEnd = true); specialBuf.clear;
            withinLex = false
            traverse(r)
          case t :: r => specialBuf.append(t.getString); traverse(r)
          case Nil => Nil
        }
      } else {
        toks match {
          case Tag(t, b) :: r =>
            val ltag = t.startsWith("<lex")
            val lEndTag = t.startsWith("</lex")
            val (l, attmap) = getLabelAndAttrsFromTag(t)
            if (ltag) withinLex = true
            if (lEndTag) withinLex = false
            //if (ltag && (opts.keepToks || !opts.preProc)) { specialTok = true; specialTokTag = Some(t, attmap) }
            if (ltag && !attmap.isEmpty) tokTagInfo = Some(attmap)
            if (ltag && lexAttributedTagSet) { specialTok = true }
            else if (!ltag && !lEndTag && (!opts.stripOriginalTags || !opts.tagset.isWithin(l, attmap))) os.write(t)
            traverse(r)
          case Ws(s) :: r =>
            os.write(s); traverse(r)
          case EndWs(s) :: r => // this can end a sequence without punctuation
            os.write(s); traverse(r)
          case HardEndTok(t) :: r => // End of Seq
            os.write(t); traverse(r)
          case SoftEndTok(t) :: r => traverse(Tok(t) :: r) // without end space, just continue
          case a :: r =>
            createNewTok(a.getString)
            //if (opts.preProc) createNewTok(a.getString) else os.write(a.getString)
            traverse(r)
          case Nil =>
        }
      }
    }
    traverse(d.elements)
    os.flush
  }
}
