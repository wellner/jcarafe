/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap

class WordScores(val file: Option[java.io.File]) extends HashMap[Long,Double] {
  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  def this(d: String) = this(Some(new java.io.File(d)))
  def this() = this(None)
  file match {case Some(f) => build(f) case None => }
  private def build (f: java.io.File) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach {l =>
      l.split('\t').toList match {
	case wd :: sc :: _ =>
	  val code = if (isNumberString(wd)) numSpecialHash else hash(wd)
	  this += (code -> sc.toDouble)
	case _ =>}
    }	
  }
}
