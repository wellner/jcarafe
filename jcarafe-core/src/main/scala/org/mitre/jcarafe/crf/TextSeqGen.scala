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
  
  lazy val indexed = elements.toIndexedSeq[Element]

  def getSlice(s: Int, e: Int) = new TextSeqDeserialization(indexed.slice(s,e).toList)
}

object TagParser extends RegexParsers {
  def tagExpr: Parser[AbstractLabel] = pref ~> elem ~ atts ~ suff ^^ {case (e~a~_) => Label(e,a)}
  def pref: Parser[String] = "</" | "<" 
  def suff: Parser[String] = "/>" | ">"
  def elem: Parser[String] = "[^ \"\'<>]+".r 
  def att: Parser[(String,String)] = "[^ \"\'<>=]+".r ~ "=" ~ "\"" ~ "[^\"<>]+".r ~ "\"" ^^ {case (a~_~_~v~_) => (a,v)}
  def atts: Parser[Map[String,String]] = rep(att) ^^ {l => l.foldLeft(Map():Map[String,String]){case (ac,kv) => ac + kv}}
  def parseIt(r: java.io.Reader) : AbstractLabel = parse(tagExpr,new PagedSeqReader(PagedSeq.fromReader(r))).getOrElse(SLabel(""))
  def parseString(s: String) : AbstractLabel = parseIt(new java.io.StringReader(s))
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
  
  def deserializeFromFile(file: String) : DeserializationT = new TextSeqDeserialization(if (opts.rawDecode) FastTokenizer.parseFileNoTags(file) else FastTokenizer.parseFile(file))
  
  def deserializeFromString(s: String) : DeserializationT = new TextSeqDeserialization(if (opts.rawDecode) FastTokenizer.parseStringNoTags(s) else FastTokenizer.parseString(s))

  def deserializeFromTokenSeq(seq: Seq[String]) : DeserializationT = new TextSeqDeserialization((seq map {el => Tok(el)}).toList)
   
  def deserializeFromRawString(s: String) : DeserializationT = deserializeFromString(s)
  
  def getLabelAndAttrsFromTag(t: String) : (String, Map[String,String]) = {
    TagParser.parseString(t) match {
      case Label(l,atts) => (l,atts)
	case _ => (t,Map()) // throw new RuntimeException("Unexpected/malformed label and attributes: " + t)
    }
  }
  
  def toSources(d: DeserializationT) : Seqs = {
    val sourceBuffer : ListBuffer[SourceSequence[String]] = new ListBuffer
    var state = other
    var curAtts : Map[String,String] = Map.empty 
    var tmpBuf = new ListBuffer[ObsSource[String]]
    var ignore = false // flag whether we're within an ignore block
    var specialTok = false // flag whether we're within a special token
    var uncertain = false // flag indicating we're in a region without certainty 
    var curPos = 0 // keeps current position within each sequence
    val specialBuf = new StringBuilder
    var localDeserialBuf = new ListBuffer[Element]
    var posWithinTag = 0 // check whether we're just inside a tag
    var docPos = 0 // position within entire deserialization; used to split out tokens
    var prevPos = 0 // end of previous sequence
    def endSeq(t: Option[String], c: Boolean) = {
      t match {case Some(t) => tmpBuf += createSource(getState(state,!c),t,!c,curAtts); curAtts = Map.empty case None => }
      if (tmpBuf.size > 0) {
        sourceBuffer += createSourceSequence(tmpBuf.toIndexedSeq,prevPos,docPos)
        curPos = 0
        tmpBuf = new ListBuffer[ObsSource[String]]
        prevPos = docPos
      }
    }
    def getAState(l: AbstractLabel, v: Boolean) = if (addBeginStates) getState(l,v) else l
    def gather (toks: List[Element], cont: Boolean) : Unit = {
      docPos += 1
      if (ignore) 
        toks match {
          case Tag("</IGNORE>", false) :: r => ignore = false; gather(r,cont)
          case _ :: r => gather(r,cont)
          case Nil => Nil
        }
      else if (specialTok) {
        toks match {
          case Tag("</lex>",_) :: r =>
            val beg = posWithinTag < 1
            val st = getAState(state,(posWithinTag < 1))
            posWithinTag +=1 
            specialTok = false; tmpBuf += createSource(st,specialBuf.toString,beg,curAtts); curAtts = Map.empty; gather(r,true) 
          case t :: r => specialBuf.append(t.getString); gather(r,cont)
          case Nil => Nil
        }
      } else {
      toks match {
    	case Tag("<BREAK_IGNORE>",true) :: r =>
    	   endSeq(None,cont); gather(r,cont) 
        case Tag("</BREAK_IGNORE>", false) :: r => endSeq(None,cont); ignore = false; gather(r,cont)    	     
        case Tag("<IGNORE>",true) :: r => ignore = true; gather(r,cont)
        case Tag(t,true) :: r => 
          val (l,attmap) = getLabelAndAttrsFromTag(t) // parse tag to get label and attribute value pairs
          if (l == "lex") {
            val attmapString: String = attmap.foldLeft(""){(ac,v) => ac + v._1 + "=" + v._2} 
            specialBuf.clear
            curAtts = attmap
            specialTok = true
          }
          opts.tagset.getTag(l,attmap) match {
                case Some(lab) =>
                  posWithinTag = 0
                  state = lab; curAtts = attmap
                case None =>
              }
          if (opts.boundaries.isWithin(l,attmap)) {endSeq(None,cont); ignore = false}
          gather(r,false)
        case HardEndTok(t) :: r => 
          if (opts.preProc) endSeq(Some(t),cont)
          gather(r, cont)
        case EndWs(t) :: r =>  endSeq(None,cont); gather(r,cont)
        case Ws(_) :: r => if ((docPos - prevPos) > MAX_SEQ_LEN) endSeq(None,cont); gather(r,cont)
        case Tag(t,false) :: r =>
          val (l,atts) = getLabelAndAttrsFromTag(t)
          if (!ignore && opts.boundaries.labelMatch(l)) {endSeq(None,cont)}
          state = other
          gather(r,false)
        case SoftEndTok(t) :: Ws(w) :: EndWs(w1) :: r => 
          if (opts.preProc) {endSeq(Some(t),cont); docPos += 2;}
          gather(r,cont)
        case SoftEndTok(t) :: EndWs(w) :: r => 
          if (opts.preProc) {endSeq(Some(t),cont); docPos += 1} 
          gather(r,cont)
        case SoftEndTok(t) :: Ws(w) :: r => 
          if (opts.preProc) {endSeq(Some(t),cont); docPos += 1} 
          gather(r,cont)          
        case SoftEndTok(t) :: r => 
          if (opts.preProc) {
        	posWithinTag += 1 
        	tmpBuf += createSource(getAState(state,!cont),t,!(cont),curAtts)
        	curAtts = Map.empty} 
          gather(r,true)  // add to queue          
        case Tok(t) :: r =>
          if (opts.preProc) {
            posWithinTag += 1
            tmpBuf += createSource(getAState(state,!cont),t,!(cont),curAtts)
            curAtts = Map.empty}
          gather(r,true)  // add to queue
        case ComplexTok(t,_) :: r => 
          if (opts.preProc) {
        	posWithinTag += 1 
	        tmpBuf += createSource(getAState(state,!cont),t,!(cont),curAtts)
	        curAtts = Map.empty} 
            gather(r,true)  // add to queue
        case _ :: r => gather(r,cont)
        case Nil => sourceBuffer += createSourceSequence(tmpBuf.toIndexedSeq,prevPos,docPos)  // add last buffer, even if its empty  
      }	
      }
    }
    gather(d.elements,false)
    sourceBuffer.toIndexedSeq
  }
  
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT =
    throw new RuntimeException("This method unsupported for Text mode currently")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[InstanceSequence]) : Map[AbstractLabel,ListBuffer[Annotation]] =
	throw new RuntimeException("Unsupported method: seqsToAnnotations")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) : Unit = 
    seqsToStream(d, seqs, new java.io.FileOutputStream(f))
  
  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]) : String = {
    val os = new java.io.ByteArrayOutputStream()
    seqsToStream(d, seqs, os)
    os.toString("UTF-8")
  } 

  def writeTok(write: Boolean, t: String, os: java.io.OutputStreamWriter) : Unit = 
    if (opts.keepToks && write) { os.write("<lex>"); os.write(t); os.write("</lex>") } else os.write(t)
  
  def seqsToAttributedDeserialization(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = {
    val ebuf = new scala.collection.mutable.ListBuffer[Element]     		
    seqs foreach {seq =>
	    val nd = d.getSlice(seq.st, seq.en)
	    val d1Els : List[Element] = seqToAttributedDeserialization(nd,seq).elements
	    ebuf ++ d1Els}
    new TextSeqDeserialization(ebuf.toList)
    }	
  
  def seqToAttributedDeserialization(d: DeserializationT, instSeq: InstanceSequence) : DeserializationT = {
    var cs = 0
    var cp = 0
    val lexInd = lAlphabet.update(SLabel("lex"))
    var curLab = lexInd
    var labSeq = instSeq.iseq
    var specialTok = false
    val lbuf = new scala.collection.mutable.ListBuffer[Element]
    val specialBuf = new StringBuilder
    
    def traverse(toks: List[Element]) : Unit = {
      if (ignoreFlag) {
        toks match {
          case Tag("</IGNORE>",_) :: r => ignoreFlag = false; traverse(r)
          case _ :: r => traverse(r)
          case Nil => Nil
        }
      } else if (specialTok) {
        toks match {
          case Tag("</lex>",_) :: r => 
            specialTok = false
            val ilab = labSeq(cp).label
            val lab = invLa(ilab) 
            if (ilab != lexInd) lbuf += ComplexTok(specialBuf.toString,Map("pre" -> lab.toString))
            specialBuf.clear
            cp += 1	
            traverse(r)
          case a :: r => specialBuf.append(a.getString); traverse(r)
          case Nil => Nil
        }
      }
      toks match {
        case Tag(t,_) :: r if t.startsWith("<lex") => specialTok = true; traverse(r)
        case Tag("<IGNORE>",_) :: r => ignoreFlag = true; traverse(r)
        case Tag(t,b) :: r => 
          if (t.startsWith("<lex") && printExistingTags) {specialTok = true}
          if (!b) { // if it's a close tag and a boundary tag, set ignore to true
        	  val (l,_) = getLabelAndAttrsFromTag(t)
        	  if (!ignoreFlag && opts.boundaries.labelMatch(l)) ignoreFlag = true
          }
          traverse(r)
        case EndWs(s) :: r => // this can end a sequence without punctuation
          lbuf += EndWs(s)
          traverse(r)
        case HardEndTok(t) :: r => // End of Seq
          lbuf += HardEndTok(t)
          traverse(r)
        case SoftEndTok(t) :: Ws(w) :: EndWs(w1) :: r => // End, with some trailing whitespace to consume
          lbuf += SoftEndTok(t)
          lbuf += Ws(w)
          lbuf += EndWs(w1)
          traverse(r)
        case SoftEndTok(t) :: EndWs(w) :: r => // End of Seq
          lbuf += SoftEndTok(t)
          lbuf += Ws(w)
          traverse(r)
        case SoftEndTok(t) :: r => // End of Seq
          lbuf += SoftEndTok(t)
          traverse(r)          
        case Tok(t) :: r =>
          val ilab = labSeq(cp).label
          val lab = invLa(ilab) 
          if (ilab != lexInd) lbuf += ComplexTok(t,Map("pre" -> lab.toString))
          cp += 1	
          traverse(r)
        case a :: r => lbuf += a; traverse(r)  
        case Nil => 
      }
      }
    traverse(d.elements)
    new TextSeqDeserialization(lbuf.toList)
  }

  def seqsToStream(d: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream) : Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    seqsToWriter(d,seqs,os)
    os.close
  }    
  
  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = {
	  seqs foreach {seq =>
	    val nd = d.getSlice(seq.st, seq.en)
	    seqToWriter(nd,seq,os)}
  }

  def seqToWriter(d: DeserializationT, iSeq: InstanceSequence, os: java.io.OutputStreamWriter) : Unit = {
    if (addBeginStates) StateCache.updateStateCache(lAlphabet) // make sure the state cache is updated for handling BEGIN states
    var cs = 0
    var cp = 0
    val lexInd = lAlphabet.update(SLabel("lex"))
    var curLab = lexInd
    var specialTok = false
    val specialBuf = new StringBuilder
    var specialTokTag : Option[String] = None
    val labSeq = iSeq.iseq

    def createNewTok(t: String, lexEnd: Boolean = false) = {
      if (cp < labSeq.length) {
        val ilab = labSeq(cp).label
        val lab = invLa(ilab) 
        val nlabState = lab match {case BeginState(l) => l case a => a}
        val normLab = if (addBeginStates) lAlphabet.update(nlabState) else ilab
        val isSpecialLex = {nlabState match {case Label(l,_) => l == "lex" case _ => false }} // always wrap each token in this case
        if ((((ilab != curLab) || (ilab != normLab)) && (ilab != lexInd)) || isSpecialLex) {
    	  os.write(lab.labelTag(false))
	  val writeLex = specialTokTag match {case Some(t) => os.write(t); false case None => true} // print out lex tags here
	  writeTok(writeLex,t,os)
	}
	else if (!isSpecialLex) {
          val writeLex = specialTokTag match {case Some(t) => os.write(t); false case None => true} // print out lex tags here
          writeTok(writeLex,t,os)
	} else writeTok(false,t,os)
        if (!isSpecialLex && lexEnd) { os.write("</lex>")}
        if (ilab != lexInd) {
    	  if (cp < labSeq.length - 1) {
    	    val nlab = labSeq(cp+1).label
            if (((nlab != ilab) && (nlab != normLab)) || StateCache.isBegin(nlab) || isSpecialLex) os.write(lab.labelTag(true))}
            else os.write(lab.labelTag(true))}
        cp += 1	
        curLab = normLab
      }	
    }
    
    def traverse(toks: List[Element]) : Unit = {
      if (ignoreFlag) 
	    toks match {
          case Tag("</IGNORE>", false) :: r => os.write("</IGNORE>"); ignoreFlag = false; traverse(r)
          case Tag("</BREAK_IGNORE>", false) :: r => os.write("</BREAK_IGNORE>"); ignoreFlag = false; traverse(r)
          case Tag(t,_) :: r => 
            val (l,atts) = getLabelAndAttrsFromTag(t)
            if (opts.boundaries.isWithin(l,atts)) ignoreFlag = false
            os.write(t)
            traverse(r)
          case t :: r => os.write(t.getString); traverse(r) 
          case Nil =>                                                 
        }
     else if (specialTok) {
       toks match {
         case Tag("</lex>",_) :: r => specialTok = false; createNewTok(specialBuf.toString, lexEnd=true); specialTokTag = None; specialBuf.clear; traverse(r)
         case t :: r => specialBuf.append(t.getString); traverse(r)
         case Nil => Nil
       }
      } 	
     else {
      toks match {
        case Tag("<IGNORE>",_) :: r => if (printExistingTags) os.write("<IGNORE>"); ignoreFlag = true; traverse(r)
        case Tag("<BREAK_IGNORE>",true) :: r => if (printExistingTags) os.write("<BREAK_IGNORE>"); ignoreFlag = true; traverse(r)
        case Tag(t,b) :: r => 
	  val ltag = t.startsWith("<lex") || t.startsWith("</lex")
          //if (ltag && opts.keepToks) {specialTok = true; specialTokTag = Some(t)}
	  if (ltag) { specialTok = true; specialTokTag = Some(t)}
          else if (printExistingTags && !ltag) os.write(t)
          if (!b) { // if it's a close tag and a boundary tag, set ignore to true
        	  val (l,_) = getLabelAndAttrsFromTag(t)
        	  if (!ignoreFlag && opts.boundaries.labelMatch(l)) ignoreFlag = true
          }
          traverse(r) 
        case Ws(s) :: r => 
          os.write(s); traverse(r)
        case EndWs(s) :: r => // this can end a sequence without punctuation
          os.write(s); traverse(r)
        case HardEndTok(t) :: r => // End of Seq
          traverse(r)
        case SoftEndTok(t) :: Ws(w) :: EndWs(w1) :: r => // End, with some trailing whitespace to consume
          os.write(w);
          os.write(w1);
          traverse(r)
        case SoftEndTok(t) :: EndWs(w) :: r => // End of Seq
          os.write(w)
          traverse(r)
        case SoftEndTok(t) :: Ws(w) :: r => // End of Seq
          os.write(w)
          traverse(r)          
        case SoftEndTok(t) :: r => traverse(Tok(t) :: r) // without end space, just continue
        case IgnoreBlock(t) :: r => os.write(t); traverse(r)
        case a :: r =>
          if (opts.preProc) createNewTok(a.getString) else os.write(a.getString)
          traverse(r)
        case Nil => 
      	}
      }
    }		
    traverse(d.elements)
    os.flush
  } 
}

trait StreamingDecoder extends FactoredDecodingSeqGen[String] with TextSeqGen {

  def seqsToSlicedWriter(d: DeserializationT, dseq: SourceSequence[String], iseqs: InstanceSequence, os: java.io.OutputStreamWriter) : Unit = 
    seqsToWriter(d.getSlice(dseq.st,dseq.en),Seq(iseqs),os)
/*
  override def extractFeatures (dseq: SourceSequence[String]) : InstanceSequence = {
    frep.otherIndex_=(otherIndex match {case Some(v) => v case None => -1}) // book-keeping to tell FeatureRep which index to not consider for segments longer than 1
    var sid = 0
    val iseq = 
      Vector.tabulate(dseq.length){(i: Int) => 
          if (dseq(i).beg) sid += 1
          val inst = frep.createInstance(dseq(i).label,dseq(i).label,sid)
          inst}
    var pos = 0
    iseq foreach {inst => frep.applyFeatureFns(inst,dseq,pos); pos += 1}
    new InstanceSequence(iseq, dseq.st, dseq.en)
  }


  override def applyDecoder(dobj: DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]) : Unit = {
    val srcs = toSources(dobj)
    outFile match {
      case Some(oFile) => 
	    val ostr = new java.io.FileOutputStream(oFile)
	    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
	    srcs foreach {srcS =>
	      val srcSInst = extractFeatures(srcS)
	      decoder.assignBestSequence(srcSInst.iseq)
	      //srcSInst foreach {l => print(l.label + " ")}; println("") 
	      seqToWriter(dobj.getSlice(srcS.st,srcS.en),srcSInst,os)
	      }		      
	    os.close
      case None => throw new RuntimeException("Expected output file")
    }
  }
*/
  /*

  override def applyDecoderParallel(dobj: DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]) : Unit = {
    val srcs = toSources(dobj)
    outFile match {
      case Some(oFile) => 
	    val ostr = new java.io.FileOutputStream(oFile)
	    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
            val futures = for ((f,i) <- srcs.zipWithIndex) yield decoderWorkers(i % numDecWorkers) !! ((srcs(i), decoder))
            val iseqs : Seq[InstanceSequence] = 
	      futures map {f => f() match {case r: InstanceSequence => r case _ => throw new RuntimeException("Future failed")}}
            iseqs.zip(srcs) foreach {case (iseq,srcS) => seqToWriter(dobj.getSlice(srcS.st, srcS.en),iseq,os)}
            os.close
      case None => throw new RuntimeException("Expected output file")}
  }
*/

}