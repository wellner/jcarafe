package org.mitre.jcarafe.util

/*
 * A compact representation for a SparseVector (more so than Map[Int, Double])
 * @param indices - index values for non-zero vector components
 * @param values - values for non-zero components at corresponding index
 */
class SparseVector(val indices: Array[Int], val values: Array[Double]) extends Serializable {
  
  require(indices.length == values.length)
  
  val length = indices.length
  
  def map(f: Double => Double) = {
    new SparseVector(indices, (values map {x => f(x)}))
  }
  
  def printVec() = {
    for (i <- 0 until length) {
      print("("+indices(i)+","+values(i)+")")
    }
  }
    

  def add(sp: SparseVector) = {
    val nindices = new collection.mutable.ArrayBuffer[Int]
    val data = new collection.mutable.ArrayBuffer[Double]
    val ln1 = this.indices.length
    val ln2 = sp.indices.length
    var curI = -1
    var i1 = 0
    var i2 = 0
    while (i1 < ln1 || i2 < ln2) {
      val iVal1 = if (i1 < ln1) this.indices(i1) else -1
      val iVal2 = if (i2 < ln2) sp.indices(i2) else -1
      if (iVal1 >= 0 && iVal2 >= 0) { 
        if (iVal1 == iVal2) {// both have this index
          nindices append iVal1
          data append (this.values(i1) + sp.values(i2))
          i1 += 1
          i2 += 1
        }
        else if (iVal1 < iVal2) { // 
          nindices append iVal1
          data append (this.values(i1))
          i1 += 1
        } else {
          nindices append iVal2
          data append (sp.values(i2))
          i2 += 1
        }
      } else if (iVal1 >= 0) {
        nindices append iVal1
          data append (this.values(i1))
          i1 += 1
      } else {
          nindices append iVal2
          data append (sp.values(i2))
          i2 += 1
      }
    }
    new SparseVector(nindices.toArray, data.toArray)
  }
}

object SparseVector {
  
  def apply(indices: Array[Int], data: Array[Double]) = new SparseVector(indices, data)
  def apply(mp: Map[Int,Double]) = {
    val sortedPairs = mp.toList.sortWith({(a,b) => a._1 < b._1}).toArray
    val ss = sortedPairs.size
    val indices = Array.fill(ss)(0)
    val data = Array.fill(ss)(0.0)
    var i = 0
    while (i < ss) {
      val (ind,v) = sortedPairs(i)
      indices(i) = ind
      data(i) = v
      i += 1
    }
    new SparseVector(indices,data)    
  }
}

object TestSparseVec {
  
  def main(args: Array[String]) = {
    val v1 = SparseVector(Array(1,3,5), Array(1.0,1.0,1.0))
    val v2 = SparseVector(Map(5 -> 1.0, 8 -> 1.0, 10 -> 1.0))
    val v3 = v1.add(v2)
    v3.printVec
    
    val w1 = SparseVector(Map())
    val w2 = SparseVector(Map(2 -> 1.0))
    val w3 = w1.add(w2)
    w3.printVec
  }
}

