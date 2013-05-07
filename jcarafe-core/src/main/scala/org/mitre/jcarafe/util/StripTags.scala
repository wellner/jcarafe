package org.mitre.jcarafe.util

import org.mitre.jcarafe.lexer._
import GenTokerConstants._
import java.io._

object StripTags {

  def main(args: Array[String]) = {
    if (args.length == 2) {
      val sr = new java.io.FileInputStream(args(0))
      val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(new java.io.File(args(1)))), "UTF-8")
      val parser = new GenToker(sr)
      var c = true
      while (c) {
	val t : org.mitre.jcarafe.lexer.Token = parser.getNextToken()
	t.kind match {
          case EOF => c = false
          case TAGSTART => 
          case TAGEND => 
          case _ => os.write(t.image)
	}
      }
    } else println("usage: java -cp jcarafe...jar org.mitre.jcarafe.util.StripTags <input file> <output file>")
  }


}
