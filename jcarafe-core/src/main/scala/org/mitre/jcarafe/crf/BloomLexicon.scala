/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import collection.immutable.BitSet
import collection.mutable.HashMap
import org.mitre.jcarafe.util.FastLoops._

/*
 * 
 */
class BloomLexicon(val dir: Option[java.io.File]) {
  def this(d: String) = this(Some(new java.io.File(d)))
  def this(d: java.io.File) = this(Some(d))
  def this() = this(None)
  val bloomTable = new HashMap[Long, BloomFilter]

  dir match {
    case Some(d) =>
      if (d.exists) {
        d.listFiles foreach { f: java.io.File =>
          if (f.isFile) {
            val fn = lexName(f.getName)
            val bf = BloomFilter(f)
            bloomTable.update(fn, bf)
          }
        }
      } else throw new RuntimeException("Lexicon directory not found")
    case None =>
  }

  def lexName(k: String) = IncrementalMurmurHash.hash(k)

  def add(k: String, el: String) = {
    bloomTable.get(lexName(el)) match { case Some(bf) => bf.add(k) case None => }
  }

  def get(l: Long): Option[List[Long]] = {
    val s = bloomTable.foldLeft(Nil: List[Long]) { case (ac, (cat, bf)) => if (bf.contains(l)) cat :: ac else ac }
    s match { case Nil => None case a => Some(a) }
  }

  def get(st: String): Option[List[Long]] = {
    val s = bloomTable.foldLeft(Nil: List[Long]) { case (ac, (cat, bf)) => if (bf.contains(st)) cat :: ac else ac }
    s match { case Nil => None case a => Some(a) }
  }

}

object BloomFilter {
  def apply(file: java.io.File) : BloomFilter = {
    val nelements = countEntriesInLexiconFile(file)
    val width = (-(nelements: Double) * math.log(0.001) / (math.log(2.0) * math.log(2.0))).toInt
    val size = (0.7 * width / nelements).toInt max 1
    val bf = new BloomFilter(size,width)
    bf.buildFromFile(file)
    bf
  }
  
  def apply(lst: Seq[Long]) : BloomFilter = {
    val nelements = lst.length
    val width = (-(nelements: Double) * math.log(0.001) / (math.log(2.0) * math.log(2.0))).toInt
    val size = (0.7 * width / nelements).toInt max 1
    val bf = new BloomFilter(size,width)
    bf.buildFromLongSeq(lst)
    bf
  } 
  
  def countEntriesInLexiconFile(f: java.io.File) : Int = {
    var nelements = 0
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach { l =>
      nelements += 1
      forIndex(l.length) { i => if (l(i) == ' ') nelements += 1 }
    }
    nelements
  }
  
  def baseHash(s: String) = IncrementalMurmurHash.hash(s)

}

class BloomFilter(val size: Int, val width: Int) {
  
  //val filter = new collection.mutable.BitSet
  val filter = new java.util.BitSet()

  import org.mitre.jcarafe.crf.IncrementalMurmurHash.mix

  private def getHash(l: Long, i: Int): Int =
    (math.abs(mix(l, i.toLong) % width)).toInt

  def contains(s: String): Boolean = contains(BloomFilter.baseHash(s))

  def contains(k: Long): Boolean = {
    var c = true
    var i = 0
    while (c && i < size) {
      c = filter.get(getHash(k, i))
      i += 1
    }
    c
  }
  
  def add(l: Long) : Unit = {
    var i = 0; while (i < size) {    
      filter.set(getHash(l, i))
      i += 1
    }
  }

  def add(e: String) : Unit = {
    val l = BloomFilter.baseHash(e)
    add(l)
  }
  
  def buildFromLongSeq(ls: Seq[Long]) : Unit = {
    ls foreach add
  }

  def buildFromFile(f: java.io.File, concatLines: Boolean = true) = {
    val src = scala.io.Source.fromFile(f)("UTF8").getLines()
    src foreach { l =>
      val els = l.split(' ')
      var mx = 0L
      val ln = els.length
      forIndex(ln) { i =>
        val ss = els(i)
        mx = IncrementalMurmurHash.mix(ss,mx)
        add(ss)
      }
      if (ln > 1) add(mx) // if multi-word term, add in mixed/conjunction value
    }
  }
}

class BloomFilterBuilder(val err: Double) {
  var elements = Set[Long]()
  def add(el: Long) = elements += el
  def add(s: String) = {
    elements += BloomFilter.baseHash(s)
  }
  
  def createBloomFilter : BloomFilter = {
    val nelements = elements.size
    val width = (-(nelements: Double) * math.log(err) / (math.log(2.0) * math.log(2.0))).toInt        
    
    val size = (0.7 * width / nelements).toInt max 1

    val bf = new BloomFilter(size, width)
    elements foreach bf.add
    bf
  }
}
