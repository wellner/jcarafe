/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.util.{Options,AbstractLabel}
import org.mitre.jcarafe.crf.AbstractInstance

class FeatureAnalyzer(opts: Options) {

  private def log2(v: Double) = math.log(v) / math.log(2.0)

  private def countValue(ar: Array[Int], dim: Int, vl: Int) = {
    var i = 0
    var sum = 0
    while (i < dim) {
      if (ar(i) == vl) sum += 1
      i += 1
    }
    sum
  }

  private def jointCount(ar1: Array[Int], ar2: Array[Int], ar1Val: Int, dim: Int) = {
    var i = 0
    var cnt = 0
    while (i < dim) {
      if ((ar1(i) == ar1Val) && ar2(i) > 0) cnt += 1
      i += 1
    }
    cnt
  }

  private def empiricalMutualInfo(ys: Array[Int], yValCounts: Array[Int], xs: Array[Int]) = {
    var mi = 0.0
    val dim = xs.length
    val xsum = countValue(xs,dim,1)
    var jcnts = List[(Int,Int)]()
    val perClassEntropies = 
    for (i <- 0 until yValCounts.length) yield {
      val jcnt = jointCount(ys, xs, i, dim)
      val jointProb = (jcnt.toDouble / dim)
      val xProb = xsum.toDouble / dim
      val yProb = yValCounts(i).toDouble / dim
      jcnts = (i,jcnt) :: jcnts
      val pmi = if ((jointProb > 0.0) && (xProb > 0.0) && (yProb > 0.0)) log2((jointProb) / (xProb * yProb)) else 0.0
      mi += (jointProb * pmi)
      (pmi / -(log2(math.max(xProb,yProb))))
    }
    (mi,xsum,jcnts,perClassEntropies)
  }

  def analyzeFeatures() = {
    val sGen = new MaxEntTrainingSeqGen(opts)
    val frep = sGen.frep
    val input: Seq[AbstractInstance] = sGen.createSeqsFromFiles(0).iseq
    val dim = input.length
    val ys = (input map {_.label}).toArray
    val yValCnts : Array[Int] = (for (i <- 0 until sGen.getNumberOfStates) yield countValue(ys,dim,i)).toArray
    val ostr = new java.io.FileOutputStream(new java.io.File(opts.outputFile.get))
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val invFMap = frep.fMap.getInvMap
    val invLaMap = sGen.invLa
    val sortedLaMap = invLaMap.toList.sortWith({case ((v1,_),(v2,_)) => v1 < v2})
    val featureMis : IndexedSeq[(Double,Int,Int,List[(Int,Int)],IndexedSeq[Double])] = 
      for (i <- 0 until (sGen.getNumberOfFeatures / sGen.getNumberOfStates)) yield {
	val ar = Array.fill(dim)(0)
	input.zipWithIndex foreach {case (ai,j) => 
	  ai.getCompactVec foreach {f => if (f.fid == i) ar(j) = 1}}
	val (mi,xsum,jcnts,perLabelEntropies) = empiricalMutualInfo(ys, yValCnts, ar)
	(mi,i,xsum,jcnts,perLabelEntropies)
      }
    val sorted = featureMis.sortWith({case ((v1,_,_,_,_),(v2,_,_,_,_)) => v1 > v2})
    os.write("Feature "); 
    os.write("MI ");
    os.write("Count ");
    sortedLaMap foreach {case (i,al) => os.write(al.toString); os.write("_MI ")}
    os.write('\n')
    sorted foreach {case (vl,fid,fcnt,jcnts,perLabelEntropies) => 
      FeatureId.fMapping.get(invFMap(fid)) match {
	case Some(fstr) =>
	  os.write(fstr); os.write(' '); os.write(vl.toString); os.write(' '); os.write(fcnt.toString); 
	  perLabelEntropies foreach {sc => os.write(' '); os.write(sc.toString)}
	  os.write('\n')
	case None => }
    }
    os.close()
  }
}

object AnalyzeFeatures {
  def main (argv: Array[String]) : Unit = {
    val opts = new Options(argv)
    val analyzer = new FeatureAnalyzer(opts)
    FeatureId.maintainMapping_=(true)
    analyzer.analyzeFeatures()
  }
}
