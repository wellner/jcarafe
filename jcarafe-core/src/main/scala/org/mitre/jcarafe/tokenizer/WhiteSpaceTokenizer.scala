/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.tokenizer

import org.mitre.jcarafe.util.CommandLineHandler 
import org.mitre.jcarafe.lexer._
import java.io.InputStream
import java.io.ByteArrayInputStream



object WhiteSpaceTokenizer {
  
  import CharStr._
  
  private def parseWithTags(scs: InputStream) = {
    import WhiteTagTokerConstants._
    val parser = new WhiteTagToker(scs)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    var c = true
    while (c) {
      val t : Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case TAGSTART => tbuf append Tag(t.image,true)
        case TAGEND => tbuf append Tag(t.image, false)
        case TOK => tbuf append Tok(t.image)
        case WHITE => tbuf append Ws(t.image)
        case WHITEEND => tbuf append EndWs(t.image)
        case _ => tbuf append Tok(t.image)
      }
    }
    tbuf.toList
  }
  
  private def parseNoTags(scs: InputStream) = {
    import WhiteSpaceTokerConstants._
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

  private def parse(scs: InputStream, parseTags: Boolean = false) = {
    if (parseTags) parseWithTags(scs) else parseNoTags(scs)
  }
  
  private def printTok(at: Boolean, s: String, os: java.io.OutputStreamWriter) = {
    if (at) os.write("<lex>")
    os.write(s)
    if (at) os.write("</lex>")
  }
  
  private def parseToFileTags(scs: InputStream, os: java.io.OutputStreamWriter) = {
    import WhiteTagTokerConstants._
    val parser = new WhiteTagToker(scs)
    var c = true
    while (c) {
      val t : Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case TAGSTART => os.write(t.image)
        case TAGEND => os.write(t.image)
        case TOK => printTok(true,t.image,os)
        case WHITE => printTok(false,t.image,os)
        case WHITEEND => printTok(false,t.image,os)
        case _ => printTok(true,t.image,os)
      }
    }
  }
  
  private def parseToFileNoTags(scs: InputStream, os: java.io.OutputStreamWriter) {
    import WhiteSpaceTokerConstants._
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
  
  private def parseToFile(scs: InputStream, os: java.io.OutputStreamWriter, parseTags : Boolean = false) = {
    if (parseTags) parseToFileTags(scs,os) else parseToFileNoTags(scs,os)
  }
  
  def parseString(s: String, parseTags: Boolean = false) = {
    parse(new ByteArrayInputStream(s.getBytes), parseTags)
  }
  
  def parseFile(f:String, parseTags: Boolean = false) = {
    val fstream = new java.io.FileInputStream(f)
    parse(fstream, parseTags)
  }
  
  def parseFileToStream(f:String,os: java.io.OutputStreamWriter, parseTags: Boolean = false) = {
    val fstream = new java.io.FileInputStream(f)
    parseToFile(fstream, os, parseTags)
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
