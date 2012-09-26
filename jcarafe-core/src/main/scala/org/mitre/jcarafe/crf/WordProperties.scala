/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap

class WordProperties(val file: Option[java.io.File]) extends HashMap[Long, List[String]] {
  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  def this(d: String) = this(Some(new java.io.File(d)))
  def this() = this(None)
  file match { case Some(f) => build(f) case None => }
  private def build(f: java.io.File) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach { l =>
      l.split(' ').toList match {
        case wd :: props =>
          this += (hash(wd) -> props)
        case Nil =>
      }
    }
  }
}
