/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap

class WordProperties(val file: Option[java.io.File]) {
  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  
   
  def this(d: String) = this(Some(new java.io.File(d)))
  def this() = this(None)
  
  val hmap = new HashMap[Long, List[String]]()
  
  file foreach build
  
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
