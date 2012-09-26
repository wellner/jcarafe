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
    //if (isSet) WhiteSpaceTokerTokenManager.ReInit(scs) else {val _ = new WhiteSpaceTokerTokenManager(scs); ()}
    val parser = new WhiteSpaceToker(scs)
    isSet = true
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
  
  def parseString(s: String) = {
    //val sr = new java.io.StringReader(s)
    //scs match {case None => scs = Some(new SimpleCharStream(sr)) case Some(cs) => cs.ReInit(sr)}
    parse(new ByteArrayInputStream(s.getBytes))
  }
  
  def parseFile(f:String) = {
    val fstream = new java.io.FileInputStream(f)
    //scs match {case None => scs = Some(new SimpleCharStream(fstream, "UTF-8")) case Some(cs) => cs.ReInit(fstream,"UTF-8")}    
    parse(fstream)
  }
	
}
