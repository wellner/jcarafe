/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.Options
import collection.mutable.HashMap

class InducedFeatureMap(mfile: Option[java.io.File] = None) extends Serializable {
  var hmap : Option[HashMap[Long,Array[Double]]] = None

  var vecSize = 0

  mfile map {mfile => 
    val mp = InducedFeatureMapProtocol.readFMap(mfile)
    vecSize = mp.head._2.length	     
    hmap = Some(mp)
  }

  def setMap(hm: HashMap[Long,Array[Double]]) = {
    vecSize = hm.head._2.length
    hmap = Some(hm)}

  def getWeightedFeatureVector(fname: Long) : Option[Array[Double]] = 
    hmap match {case Some(hm) => hm.get(fname) case None => None}
}

object InducedFeatureMap {

  def apply() : Option[InducedFeatureMap] = Some(new InducedFeatureMap())

  def apply(opts: Options) : Option[InducedFeatureMap] = {
    opts.weightedFeatureMap map {file => new InducedFeatureMap(Some(new java.io.File(file)))}
  }

  def apply(fs: String) : InducedFeatureMap = new InducedFeatureMap(Some(new java.io.File(fs)))

  def apply(hm: HashMap[Long,Array[Double]]) : InducedFeatureMap = {
    val nm = new InducedFeatureMap
    nm.setMap(hm)
    nm
  }
}
