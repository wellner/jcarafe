package org.mitre.jcarafe.tokenizer

import util.parsing.combinator.RegexParsers
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

class TokenizerPatternParser extends RegexParsers {
  
  override val whiteSpace = "[ ]+".r
  val catTbl = new collection.mutable.HashMap[String,Category]
  
  def topExprs : Parser[(Option[List[Category]],List[PatternSequence])] = 
    opt(topExprs2) ~ repsep((topExpr1|topExpr1_2),";?(\r?\n)+".r) ~ opt(";?(\r?\n)+".r) ^^ {case (te~e~_) => (te,e)}
  
  // need to add in comment options

  def topExpr1_2 : Parser[PatternSequence] = "[ #]+".r ~ "[^;\r\n]*".r ^^ {case (_~_) => new SplitPatternSequence(List())}
  def topExpr1 : Parser[PatternSequence] =
    ("SPLIT:" | "MERGE:") ~ topExpr ^^ {case (ext~exp) => 
      ext match {
        case "SPLIT:" => new SplitPatternSequence(exp)
        case "MERGE:" => new MergePatternSequence(exp)
        case _ => new SplitPatternSequence(List())
      }
    }
  
  def topExprs2 : Parser[List[Category]] = rep(topExpr2)
  
  def topExpr2 : Parser[Category] = 
    atomExpr ~ ":=" ~ repsep(strString,"""[;\n\r]+""".r) ~ "=:" ^^ {case (n~_~exprs~_) =>
      val cat = Category(n,exprs.toSet)
      catTbl.update(n,cat); cat} 
  
  def topExpr : Parser[List[PatternElement]] =
    patExpr ~ topExpr ^^ {case (h~t) => h :: t} |
    patExpr ^^ {case (el) => List(el)}
    
  def patExpr : Parser[PatternElement] = repReExpr | reExpr | tokExpr | catExpr | strExpr | recurseExpr
  
  def atomExpr : Parser[String] = """[A-z]+""".r
  
  def recurseExpr : Parser[PatternElement] = "==Recurse==" ^^ {case _ => Recurse}
  
  def repReExpr : Parser[PatternElement] = "Rep(" ~> """(\\\)|[^\)])+""".r <~ ")" ^^ {case(re) => Rep(re.r)}
    
  def reExpr : Parser[PatternElement] = "R(" ~> """(\\\)|[^\)])+""".r <~ ")" ^^ {case(re) =>
    val aRe = re.replaceAll("\\\\\\(","(").replaceAll("\\\\\\)",")") // this replaces the escaped parens with unescaped ones
    R(aRe.r)}
    
  def catExpr : Parser[PatternElement] = """[A-z0-9_\.:-]+""".r ^^ {case (ce) => catTbl.get(ce).getOrElse(Category(ce,Set()))}
  
  def strString : Parser[String] = "\"" ~> """(\\\"|[^|\"])+""".r <~ "\""
  
  def strExpr : Parser[PatternElement] = "\"" ~> """(\\\"|[^|\"])+""".r <~ "\"" ^^ {case (ee) => S(ee)}
  
  def tokExpr : Parser[PatternElement] = "TOK" ^^ {_ => Tk}
  
  def parseIt(r: java.io.Reader) : (Option[List[Category]],List[PatternSequence]) = { 
    val res = parseAll(topExprs,r)
    if (res.successful) {
      val rr = res.get
      (rr._1,rr._2.filter(ps => !(ps.seq.length < 1)))
    }
    else throw new RuntimeException("Feature Specification File failed to Parse") }
  
  def parseString(s: String) : (Option[List[Category]],List[PatternSequence]) = parseIt(new java.io.StringReader(s))
  def parseFile(f: java.io.File) : (Option[List[Category]],List[PatternSequence]) = {
    val fileReader = new java.io.FileReader(f)
    parseIt(fileReader)
  }
}