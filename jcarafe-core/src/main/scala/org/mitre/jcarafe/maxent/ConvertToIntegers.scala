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
	  val indices = features map {f =>
	    val fs = f.split(':')
	    if (fs.length > 1) ((alphabet.update(fs(0)) + 1), fs(1).toDouble)
	    else ((alphabet.update(fs(0)) + 1), 1.0)}
	  var curId = -1
	  indices.sortWith(_._1 < _._1) foreach {case (f,v) =>
	    if (f > curId) {
   	      os.write(' '); os.write(f.toString); os.write(':'); os.write(v.toString)}
	    curId = f
	    }
	  os.write('\n')
	case _ => 
      }
      }
    }
    os.flush
    os.close
  }
}
