/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.tokenizer
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

sealed abstract class Element extends Positional {
  def getString : String
}
case class Tok(s:String) extends Element { def getString = s }
case class ComplexTok(s: String, atts: Map[String,String]) extends Element { def getString = s }
/**
 * The <code>Tag</code> case class represents an SGML element.
 * @param s The tag element string (including attributes for start tags)
 * @param b A <code>Boolean</code> true if this tag is a start tag; otherwise false
 */
case class Tag(s:String,b:Boolean) extends Element { def getString = s }
case class HardEndTok(s: String) extends Element { def getString = s }
case class SoftEndTok(s: String) extends Element { def getString = s }
case class EndWs(s:String) extends Element { def getString = s }
case class Ws(s:String) extends Element { def getString = s }
case class IgnoreBlock(s:String) extends Element { def getString = s }

abstract class Tokenizer extends RegexParsers {
  override def skipWhitespace = false
  /* The 'Grammar' */
  def expr : Parser[Element] = whiteEnd | white | endPunct | hardEndPunct | abbrev | tag | tok | punct 
  def exprs: Parser[List[Element]] = rep(expr)
  def tag = tagE | tagS
  
  def tagS : Parser[Element] =  delimStartS ~ tokSeq ~ delimEnd ^^ {case (b~l~e) => Tag((b + (l + e)),true) }
  def tagE : Parser[Element] =  delimStartE ~ tokSeq ~ delimEnd ^^ {case (b~l~e) => Tag((b + (l + e)),false) } 
  
  /* The 'Lexer' */
  def delimStartS : Parser[String] = "<"
  def delimStartE : Parser[String] = "</" 
  def delimEnd : Parser[String] = ">"
  def tokSeq: Parser[String] = """[^<>]*""".r 
  def hardEndPunct : Parser[Element] = """[\u06D4\u0964]+""".r ^^ {case l => HardEndTok(l)}
  def endPunct : Parser[Element] = """\)?[\.\?!]+(\"|'')*""".r ^^ {case l => SoftEndTok(l)}
  def tok: Parser[Element]
  def abbrev: Parser[Element] 
  def white : Parser[Element]
  def whiteEnd : Parser[Element]
  def punct: Parser[Element] = """[\p{P}]+""".r ^^ {l => Tok(l)}
  def parseIt(r: java.io.Reader) = parseAll(exprs,new PagedSeqReader(PagedSeq.fromReader(r))).getOrElse(Nil)
  def parseFile(s: String) : List[Element] = parseIt(new java.io.InputStreamReader(new java.io.FileInputStream(s), "UTF-8"))
  def parseString(s: String) : List[Element] = parseIt(new java.io.StringReader(s))
}

object RawTokenizer extends Tokenizer {
  def tok: Parser[Element] = positioned {"""(-|[^\p{P}\s<])+""".r ^^ {l => Tok(l) }} // this is an individual token
  def abbrev : Parser[Element] = {
    """'s|n't|'d|'m|'re|'ll|'ve|[0-9]+\.[0-9]*|[0-9]?[0-9](:[0-9][0-9])?[Pp]\.?[Mm]\.?|&[A-Za-z0-9#]+;
       |U\.S\.|Mr\.|Ms\.|Dr\.|Gen\.|Mrs\.|Adm\.|Co\.|Inc\.|Ltd\.|Corp\.|[Pp]\.[Mm]\.|Mass\.|[A-z]\.|Calif\.
       |Adm\.|Brig\.|Capt\.|Cmdr\.|Col\.|Comdr\.|Dn\.|Dr\.|Drs\.|Geo\.|Gen\.|Gov\.|Govs\.|Hon\.|Jr\.|Lt\.|Maj\.
       |Messrs\.|Pres\.|Prof\.|Profr\.|Rep\.|Reps\.|Rev\.|Sen\.|Sens\.|Sgt\.|Sr\.|Sra\.|Srta\.|ST\. |St\.|Ste\.
       |Wm\.|Geo\.|Jos\.|Chas\.|Bancorp\.|Bhd\.|Bros\.|Cia\.|Cie\.|Co\.|Cos\.|Corp\.|Inc\.|Ltd\.|Mfg\.|Pte\.|Pty\.
       |Ala\.|Ariz\.|Ark\.|Ca\.|Calif\.|Colo\.|Conn\.|Del\.|Fla\.|Ga\.|Ind\.|Ill\.|Kan\.|Ky\.|La\.
       |Mass\.|Md\.|Mich\.|Minn\.|Miss\.|Mo\.|Mont\.|Neb\.|Nev\.|Okla\.|Ont\.|Ore\.|Penn\.|Pa\.|Tenn\.|Tex\.
       |Va\.|Vt\.|Wash\.|W\.|Wis\.|Wyo\.|Jan\.|Feb\.|Mar\.|Apr\.|Jun\.|Jul\.|Aug\.|Sep\.|Sept\.|Oct\.|Nov\.|Dec\.""".r ^^ {l => Tok(l)}
  }

  def white : Parser[Element] = 
    """[ \t\v\f]+""".r ^^ {l => Ws(l)} |
    """\r?\n""".r ~ not(white | whiteEnd) ^^ {case(l~_) => Ws(l)}
  
  def whiteEnd : Parser[Element] = (("""(\r?\n)\s+""".r) ^^ {l => EndWs(l)})

}	



