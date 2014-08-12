/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap

class WordProperties extends Serializable {
  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  
  val hmap = new HashMap[Long, List[String]]()
  private def build(f: java.io.File) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach { l =>
      l.split(' ').toList match {
        case wd :: props =>
          hmap += ((hash(wd), props))
        case Nil =>
      }
    }
  }
  
  def apply = hmap.apply _
  def get = hmap.get _
}

object WordProperties {
  
  def apply(f: Option[java.io.File]) : WordProperties = {
    val wp = new WordProperties
    f foreach wp.build
    wp
  }
  
  def apply(path: String) : WordProperties = apply(new java.io.File(path))
  
  def apply(f: java.io.File) : WordProperties = {
    val wp = new WordProperties
    wp.build(f)
    wp
  }
}