/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import collection.mutable.BitSet
import collection.mutable.HashMap
import org.mitre.jcarafe.util.FastLoops._

class BloomLexicon(val dir: Option[java.io.File]) extends Lexicon {
  def this(d: String) = this(Some(new java.io.File(d)))
  def this(d: java.io.File) = this(Some(d))
  def this() = this(None)
  val bloomTable = new HashMap[Long, BloomFilter]

  dir match {
    case Some(d) =>
      if (d.exists) {
        d.listFiles foreach {f : java.io.File => 
	  val fn = lexName(f.getName)
	  val bf = new BloomFilter(f)
          bloomTable.update(fn, bf) }			     
      } else throw new RuntimeException("Lexicon directory not found")
    case None => }

  def lexName(k: String) = IncrementalMurmurHash.hash(k)

  def add(k: String, el: String) = {
    bloomTable.get(lexName(el)) match {case Some(bf) => bf.add(k) case None => }
  }

  def get(l: Long) : Option[List[Long]] = {
    val s = bloomTable.foldLeft(Nil: List[Long]) {case (ac,(cat,bf)) => if (bf.contains(l)) cat :: ac else ac}
    s match {case Nil => None case a => Some(a)}
  }

  def get(st: String) : Option[List[Long]] = {
    val s = bloomTable.foldLeft(Nil: List[Long]) {case (ac,(cat,bf)) => if (bf.contains(st)) cat :: ac else ac}
    s match {case Nil => None case a => Some(a)}
  }

}

class BloomFilter(val file: Option[java.io.File] = None) {

  def this(f: java.io.File) = this(Some(f))

  import org.mitre.jcarafe.crf.IncrementalMurmurHash.mix

  var size = 0
  var nelements = 0
  var width = 0
  var filter = {
    file match {
      case Some(file) =>
	    count(file)
        width = (- (nelements: Double) * math.log(0.001) / (math.log(2.0) * math.log(2.0))).toInt
        size = (0.7 * width / nelements).toInt max 1
        new BitSet(width) 
      case None => new BitSet() }
  }

  file match {case Some(file) => build(file) case None => }

  def baseHash(s: String) = {
    val barr = s.getBytes("UTF-16")
    IncrementalMurmurHash.hash(barr,barr.length,0)
  }

  private def getHash(l: Long, i: Int) : Int = 
    (math.abs(mix(l,i.toLong) % width)).toInt

  def contains(s: String) : Boolean = contains(baseHash(s))

  def contains(k: Long) : Boolean = {
    var c = true
    var going = true
    var i = 0
    while (c && i < size) {
      c = filter(getHash(k,i))
      i += 1
    }
    c
  }

  def add(e: String) = {
    val l = baseHash(e)
    forIndex(size){i =>
      filter += (getHash(l,i))
    }
  }
  
  def build(f: java.io.File) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach {l => 
      val els = l.split(' ')
      forIndex(els.length){i =>	
        add(els(i))}		 
     }
  }

  def count(f: java.io.File) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach {l => 
      nelements += 1
      forIndex(l.length) {i => if (l(i) == ' ') nelements += 1 }
	       }
  }

}
