package org.mitre.jcarafe.util

import org.mitre.jcarafe.tokenizer.{ FastTokenizer, Element, Tag, Tok, Ws, EndWs }
import org.mitre.jcarafe.crf.TagParser

case class AlignSeq(tgt: Int, src: Int, prob: Double)
case class PropertyVal(vl: String, sc: Double)

class Token(prs: Map[String, String], val tokVal: String) {
    var props = prs map { case (k, v) => (k, List(PropertyVal(v, 1.0))) }

    def assignAttributes(m: Map[String, List[PropertyVal]], sc: Double = 1.0) = {
      m foreach {
        case (k, v) =>
          val is = props.get(k).getOrElse(Nil)
          val nv = if (sc < 1.0) v map { case PropertyVal(vv, ss) => PropertyVal(vv, sc) } else v
          props += (k -> (is ::: nv))
      }
    }
    def attToString(a: String, v: PropertyVal) = a + "=\"" + v.vl + "\""
    def scoreToString(v: PropertyVal) = "sc=\"" + v.sc + "\""
    private def findBestAttval(a: String) = {
      props.get(a) map { vls =>
        vls match {
          case h :: r => r.foldLeft(h) { case (cb, c) => if (c.sc > cb.sc) c else cb }
          case Nil => throw new RuntimeException("Empty attribute list")
        }
      }
    }
    def findBestAttvalOver(t: Double, a: String) = {
      findBestAttval(a) match {
        case Some(pv) => if (pv.sc > t) Some(pv) else None
        case None => None
      }
    }
    
  }
  
  
abstract class ProjectAligned {

  def tokenBestOverToString(tok: Token, t: Double, writeSc: Boolean = true) : String
  
  def getAlignSequence(l: String) = {
    if (l.length > 1) {
      l.split(' ').toIndexedSeq map { tr =>
        tr.split('-').toList match {
          case s :: t :: v :: Nil => AlignSeq(s.toInt, t.toInt, v.toDouble)
          case s :: t :: Nil => AlignSeq(s.toInt, t.toInt, 1.0)
          case _ => throw new RuntimeException("Unable to parse alignment: " + tr)
        }
      }
    } else IndexedSeq()
  }

  def getAttributes(s: String) = {
    TagParser.parseString(s) match {
      case Label(l, atts) => atts
      case _ => throw new RuntimeException("Unable to parse 'lex' tag string: " + s)
    }
  }

  def gatherLogicalTokens(elems: List[Element]): List[Token]

  def projectToTgtTokens(srcToks: IndexedSeq[Token], tgtToks: IndexedSeq[Token], alignment: IndexedSeq[AlignSeq]) = {
    alignment foreach {
      case AlignSeq(t, s, sc) => tgtToks(t).assignAttributes(srcToks(s).props, sc)
    }
  }
}

class ProjectAlignedTokenAttributes extends ProjectAligned {
  def gatherLogicalTokens(elems: List[Element]): List[Token] = {
    elems match {
      case Tag(s, true) :: el :: Tag(_, false) :: r => new Token(getAttributes(s), el.getString) :: gatherLogicalTokens(r)
      case a :: r => gatherLogicalTokens(r)
      case Nil => Nil
    }
  }
  
  def tokenBestOverToString(tok: Token, t: Double, writeSc: Boolean = true) = {
      val sbuf = new StringBuilder
      sbuf append "<lex"
      tok.props foreach {
        case (a, pvs) =>
          tok.findBestAttvalOver(t, a) foreach { pv =>
            sbuf append " "
            sbuf append tok.attToString(a, pv); if (writeSc) { sbuf append ' '; sbuf append tok.scoreToString(pv) }
          }
      }
      sbuf append ">"
      sbuf append tok.tokVal
      sbuf append "</lex>"
      sbuf.toString
    }


  def projectAttributes(srcFile: String, alignFile: String, tgtFile: String, outFile: String, th: Double = 0.5) = {
    val inSrc = io.Source.fromFile(srcFile)("UTF-8").getLines
    val inTgt = io.Source.fromFile(tgtFile)("UTF-8").getLines
    val inAlign = io.Source.fromFile(alignFile)("UTF-8").getLines
    var lnCnt = 0
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(outFile)), "UTF-8")
    inSrc foreach { srcLine =>
      val tgtLine = inTgt.next
      lnCnt += 1
      val srcFileToks = gatherLogicalTokens(FastTokenizer.parseString(srcLine, true)).toVector // true - keep lex tags in token stream
      val tgtFileToks = gatherLogicalTokens(FastTokenizer.parseString(tgtLine, true)).toVector
      val alignSequence = getAlignSequence(inAlign.next)
      if (alignSequence.length > 0) {
        try {
          projectToTgtTokens(srcFileToks, tgtFileToks, alignSequence)
        } catch {
          case _: Throwable => println("Failure: \n src: " + srcLine + "\n tgt: " + tgtLine + " on line: " + lnCnt)
        }
        os.write("<s>")
        tgtFileToks foreach { t => os.write(tokenBestOverToString(t,th)) }
        os.write("</s>")
        os.write('\n')
      }
    }
    os.flush()
  }
}

class ProjectAlignedTags extends ProjectAligned {
  
  def tokenBestOverToString(tok: Token, t: Double, writeSc: Boolean = true) = {
    
    val sbuf = new StringBuilder
    val tagElement = tok.props.get("tag")
    if (tagElement.isDefined) {
      sbuf append ('<')
      sbuf append tagElement.get.head.vl
      tok.props foreach {
        case (a, pvs) =>
          if (a != "tag") {
          tok.findBestAttvalOver(t, a) foreach { pv =>
            sbuf append " "
            sbuf append tok.attToString(a, pv); if (writeSc) { sbuf append ' '; sbuf append tok.scoreToString(pv) }
          }
          }
      }
    sbuf append ('>')
    sbuf append tok.tokVal
    sbuf append ("</")
    sbuf append tagElement.get.head.vl
    sbuf append ("> ")    
    } else {
        sbuf append tok.tokVal
        sbuf append ' '
    }
    sbuf.toString        
  }
  
  def getRemainingPhraseTokens(elems: List[Element], atts: Map[String,String]) : (List[Element],List[Token]) = {
    def getRemainingPhraseTokens(elems: List[Element], acc: List[Token]) : (List[Element],List[Token]) = {
      elems match {
        case Tag(t,false) :: r => (r,acc)
        case el :: r => getRemainingPhraseTokens(r,new Token(atts, el.getString) :: acc)
      }
    }
    getRemainingPhraseTokens(elems,Nil)
  }

  def gatherLogicalTokens(elems: List[Element]): List[Token] = {
    elems match {
      case Tag(s, true) :: rest =>
        val attsP = getAttributes(s)
        val atts = TagParser.parseString(s) match { case Label(l, _) => attsP + ("tag" -> l) case _ => attsP }
        val (remElements,toks) = getRemainingPhraseTokens(rest,atts)
        toks ++ gatherLogicalTokens(remElements)
      case Ws(_) :: r => gatherLogicalTokens(r)
      case EndWs(_) :: r => gatherLogicalTokens(r)
      case a :: r => new Token(Map(),a.getString) :: gatherLogicalTokens(r)
      case Nil => Nil
    }
  }

  def projectTags(srcFile: String, alignFile: String, tgtFile: String, outFile: String, th: Double = 0.5) = {
    val inSrc = io.Source.fromFile(srcFile)("UTF-8").getLines
    val inTgt = io.Source.fromFile(tgtFile)("UTF-8").getLines
    val inAlign = io.Source.fromFile(alignFile)("UTF-8").getLines
    var lnCnt = 0
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(outFile)), "UTF-8")
    inSrc foreach { srcLine =>
      val tgtLine = inTgt.next
      lnCnt += 1
      val srcFileToks = gatherLogicalTokens(FastTokenizer.parseString(srcLine, true)).toVector // true - keep lex tags in token stream
      val tgtFileToks = gatherLogicalTokens(FastTokenizer.parseString(tgtLine, true)).toVector
      val alignSequence = getAlignSequence(inAlign.next)
      if (alignSequence.length > 0) {
        try {
          projectToTgtTokens(srcFileToks, tgtFileToks, alignSequence)
        } catch {
          case _: Throwable =>
            println("Failure: \n src: " + srcLine + "\n tgt: " + tgtLine + " on line: " + lnCnt)
        }
        os.write("<s>")
        tgtFileToks foreach { t => os.write(tokenBestOverToString(t,th,false)) }
        os.write("</s>")
        os.write('\n')
      }
    }
    os.flush()
  }
}

object ProjectAlignedAttributes {

  def main(args: Array[String]) = {
    val th = if (args.length > 4) args(4).toDouble else 0.5
    val mapper = new ProjectAlignedTokenAttributes
    mapper.projectAttributes(args(0), args(1), args(2), args(3), th)
  }
}

object ProjectAlignedTags {
  def main(args: Array[String]) = {
    val th = if (args.length > 4) args(4).toDouble else 0.5
    val mapper = new ProjectAlignedTags
    mapper.projectTags(args(0), args(1), args(2), args(3), th)
  }
}