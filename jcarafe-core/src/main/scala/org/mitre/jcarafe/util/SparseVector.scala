package org.mitre.jcarafe.util

/*
 * A compact representation for a SparseVector (more so than Map[Int, Double])
 * @param indices - index values for non-zero vector components
 * @param values - values for non-zero components at corresponding index
 */
class SparseVector(val indices: Array[Int], val values: Array[Double]) extends Serializable {
  
  def map(f: Double => Double) = {
    new SparseVector(indices, (values map {x => f(x)}))
  }

}