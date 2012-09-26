package org.mitre.jcarafe.util

import org.mitre.jcarafe.lexer._
import org.mitre.jcarafe.tokenizer._
import GenTokerConstants._
import java.io._

object SeparateDocs {

  def main(args: Array[String]) = {
    if (args.length == 3) {
      var cnt = 0
      val dir = new File(args(0))
      val tg = args(2)
      val rr = ("<" + tg + " ").r
      val odirPath = args(1)
      dir.listFiles foreach { f =>
        val sr = new java.io.FileInputStream(f)
        var os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(new java.io.File(odirPath + f.getName + ".doc-" + cnt.toString))), "UTF-8")
        val parser = new GenToker(sr)
        var c = true
        while (c) {
          val t: Token = parser.getNextToken()
          t.kind match {
            case EOF => c = false
            case TAGSTART =>
              val ss = t.image
              if (rr.findFirstIn(ss).isDefined) {
                os.close
                cnt += 1
                os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(new java.io.File(odirPath + f.getName + ".doc-" + cnt.toString))), "UTF-8")
              }
            case TAGEND =>
            case _ => os.write(t.image)
          }
        }
        os.close
      }
    } else println("usage: java -cp jcarafe...jar org.mitre.jcarafe.util.StripTags <input file> <output file>")
  }
}

