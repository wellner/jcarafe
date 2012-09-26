/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap
import org.mitre.jcarafe.util.FastLoops._

trait Lexicon {
  
  def get(l: String) : Option[List[Long]]

  def get(l: Long) : Option[List[Long]]

  def add(k: String, el: String) : Unit

}

class HashLexicon(val dir: Option[java.io.File]) extends HashMap[String,List[Long]] with Lexicon {
  def this(d: String) = this(Some(new java.io.File(d)))
  def this(d: java.io.File) = this(Some(d))
  def this() = this(None)
  dir match {
    case Some(d) =>
      if (d.exists)
    	  d.listFiles foreach build
      else throw new RuntimeException("Lexicon directory not found")
    case None => }

  def get(l: Long) = throw new RuntimeException("Unsupported method")

  def add(k: String, el: String) = {
    val hv = IncrementalMurmurHash.hash(el)
    val curV = this.get(k).getOrElse(Nil)
    this += (k -> (hv :: curV))
  }

  def build (f: java.io.File) = {
    println("Reading in lexicon file: " + f.getPath)
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    val name = f.getName
    src foreach {l => 
      val els = l.split(' ') 
      forIndex(els.length){i =>	
	if (i > 0) {
	  val nm = name + i
	  add(els(i),nm)}
        add(els(i),name)}
      }
  }
}
