/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.Alphabet

object ConvertToIntegers {
  
  val alphabet = new Alphabet[String]
  val labAlphabet = new Alphabet[String]
  
  def main(args: Array[String]) : Unit = {
    val src = scala.io.Source.fromFile(new java.io.File(args(0)))("UTF-8")
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(new java.io.File(args(1)))))
    src.getLines foreach {l =>
      if (l.length > 1) {
      l.split(' ').toList match {
	case lab :: features =>
	  val nl = labAlphabet.update(lab)
	  os.write(nl.toString)
	  val indices = features map {f => alphabet.update(f) + 1}
	  indices.sortWith(_ < _) foreach {f => os.write(' '); os.write(f.toString); os.write(":1.0")}
	  os.write('\n')
	case _ => 
      }
      }
    }
    os.flush
    os.close
  }
}
