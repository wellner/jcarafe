/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.tokenizer

import org.mitre.jcarafe.util.CommandLineHandler 
import org.mitre.jcarafe.lexer._
import java.io.InputStream
import java.io.ByteArrayInputStream
import WhiteSpaceTokerConstants._

object WhiteSpaceTokenizer {
  
  import CharStr._

  private def parse(scs: InputStream) = {
    val parser = new WhiteSpaceToker(scs)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    var c = true
    while (c) {
      val t : Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case TOK => tbuf append Tok(t.image)
        case WHITE => tbuf append Ws(t.image)
        case WHITEEND => tbuf append EndWs(t.image)
        case _ => tbuf append Tok(t.image)
      }
    }
    tbuf.toList
  }
  
  private def printTok(at: Boolean, s: String, os: java.io.OutputStreamWriter) = {
    if (at) os.write("<lex>")
    os.write(s)
    if (at) os.write("</lex>")
  }
  
  private def parseToFile(scs: InputStream, os: java.io.OutputStreamWriter) = {
    val parser = new WhiteSpaceToker(scs)
    var c = true
    while (c) {
      val t : Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case TOK => printTok(true,t.image,os)
        case WHITE => printTok(false,t.image,os)
        case WHITEEND => printTok(false,t.image,os)
        case _ => printTok(true,t.image,os)
      }
    }
  }
  
  def parseString(s: String) = {
    parse(new ByteArrayInputStream(s.getBytes))
  }
  
  def parseFile(f:String) = {
    val fstream = new java.io.FileInputStream(f)
    parse(fstream)
  }
  
  def parseFileToStream(f:String,os: java.io.OutputStreamWriter) = {
    val fstream = new java.io.FileInputStream(f)
    parseToFile(fstream, os)
  }
  
  def main(args: Array[String]) = {
    val ifile = args(0)
    val ofile = args(1)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(ofile)), "UTF-8")
    parseFileToStream(ifile,os)
    os.flush()
    os.close()
  }
}
