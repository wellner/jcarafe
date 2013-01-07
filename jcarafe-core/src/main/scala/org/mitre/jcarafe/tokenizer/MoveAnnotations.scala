/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.tokenizer

/**
 * Utility application that takes a tagged file and moves tags outward such that
 * they do not reside inside what would be regarded as a 'token' by simple tokenizers
 */
object MoveAnnotations {
  
  val r1 = """\p{P}""".r
  private def processFile(f: java.io.File) = {
    val toks = RawTokenizer.parseFile(f.toString)
    val nf = new java.io.File(f.toString + ".conv")
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(nf)), "UTF-8")
    def traverse(toks: List[Element]) : Unit = {
      toks match {
        case Tok(t) :: Tag(tag,true) :: r 
        if (r1.findFirstIn(t) match {case Some(_) => false case None => true}) =>
          traverse(Tag(tag,true) :: Tok(t) :: r)
        case Tag(tag,false) :: Tok(t) :: r
        if (r1.findFirstIn(t) match {case Some(_) => false case None => true}) =>
          traverse(Tok(t) :: Tag(tag,false) :: r)
        case Tag(t,true) :: r => os.write("<"); os.write(t); os.write(">"); traverse(r)
        case Tag(t,false) :: r => os.write("</"); os.write(t); os.write(">"); traverse(r)
        case a :: r => os.write(a.getString); traverse(r)                                                                               
        case Nil => 
      }
    }
    traverse(toks)
    os.flush
    os.close
  }
  
  def main(args: Array[String]) = {
    val dir = new java.io.File(args(0))
    dir.listFiles foreach { f => if (f.isFile) processFile(f) }
  }
}
