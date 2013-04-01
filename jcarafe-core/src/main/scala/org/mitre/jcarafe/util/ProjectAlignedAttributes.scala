package org.mitre.jcarafe.util

import org.mitre.jcarafe.tokenizer.{ FastTokenizer, Element, Tag, Tok }
import org.mitre.jcarafe.crf.TagParser

object ProjectAlignedAttributes {

  case class AlignSeq(tgt: Int, src: Int, prob: Double)
  case class PropertyVal(vl: String, sc: Double)

  class Token(prs: Map[String, String], val tokVal: String) {
    var props = prs map { case (k, v) => (k, List(PropertyVal(v, 1.0))) }

    def assignAttributes(m: Map[String, List[PropertyVal]], sc: Double = 1.0) = {
      m foreach {
        case (k, v) =>
          val is = props.get(k).getOrElse(Nil)
          val nv = if (sc < 1.0) v map {case PropertyVal(vv,ss) => PropertyVal(vv,sc)} else v
          props += (k -> (is ::: nv))
      }
    }
    private def attToString(a: String, v: PropertyVal) = a + "=\"" + v.vl + "\""
    private def scoreToString(v: PropertyVal) = "sc=\"" + v.sc + "\""
    private def findBestAttval(a: String) = {
      props.get(a) map { vls =>
        vls match {
          case h :: r => r.foldLeft(h) { case (cb, c) => if (c.sc > cb.sc) c else cb }
          case Nil => throw new RuntimeException("Empty attribute list")
        }
      }
    }
    private def findBestAttvalOver(t: Double, a: String) = {
      findBestAttval(a) match {
        case Some(pv) => if (pv.sc > t) Some(pv) else None
        case None => None
      }
    }
    def tokenBestOverToString(t: Double, writeSc: Boolean = true) = {
      val sbuf = new StringBuilder
      sbuf append "<lex"
      props foreach {
        case (a, pvs) =>          
          findBestAttvalOver(t, a) foreach { pv =>
            sbuf append " "
            sbuf append attToString(a, pv); if (writeSc) {sbuf append ' '; sbuf append scoreToString(pv) }}
      }
      sbuf append ">"
      sbuf append tokVal
      sbuf append "</lex>"
      sbuf.toString
    }
  }

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

  def gatherLogicalTokens(elems: List[Element]): List[Token] = {
    elems match {
      case Tag(s, true) :: el :: Tag(_, false) :: r => new Token(getAttributes(s), el.getString) :: gatherLogicalTokens(r)
      case a :: r => gatherLogicalTokens(r)
      case Nil => Nil
    }
  }

  def projectToTgtTokens(srcToks: IndexedSeq[Token], tgtToks: IndexedSeq[Token], alignment: IndexedSeq[AlignSeq]) = {
    alignment foreach {
      case AlignSeq(t, s, sc) => tgtToks(t).assignAttributes(srcToks(s).props,sc)
    }
  }

  def projectAttributes(srcFile: String, alignFile: String, tgtFile: String, outFile: String, th: Double = 0.5) = {
    val inSrc = io.Source.fromFile(srcFile).getLines
    val inTgt = io.Source.fromFile(tgtFile).getLines
    val inAlign = io.Source.fromFile(alignFile).getLines
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
	tgtFileToks foreach { t => os.write(t.tokenBestOverToString(th)) }
        os.write("</s>")
        os.write('\n')
      }
    }
    os.flush()
  }

  def main(args: Array[String]) = {
    val th = if (args.length > 4) args(4).toDouble else 0.5
    projectAttributes(args(0), args(1), args(2), args(3), th)
  }
}