/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import org.mitre.jcarafe.util.Options

import scala.math.{log,exp}

trait SemiCrf extends Crf {
  protected def logSumExp(v1: Double, v2: Double) = {
    val vmin = math.min(v1, v2)
    val vmax = math.max(v1, v2)
    vmax + (log (exp(vmin - vmax) + 1))
  }
  
  protected def setArrayTo(ar: Array[Double], v: Double) = {
    var x = 0
    while (x < ar.length) { ar(x) = v; x += 1}
  }
  
  var t2: Array[Double] = Array.fill(nls)(0.0)
  
  def matrixMultLog(mat:Matrix, vec: Array[Double], rvec: Array[Double], alpha:Double, beta:Double, trans: Boolean) = {
    var i = 0 
    val lalpha = if (alpha == 1.0) 0.0 else log(alpha)
	if (beta > 0) {
		if ((beta < 1) || (beta > 1) ) {
          val lbeta = log(beta)
          while(i < rvec.length) {
            rvec(i) += lbeta
            i += 1
          }
        }
      }	else {
        i = 0;
        while(i < rvec.length) {
           rvec(i) = -(Double.MaxValue)
           i += 1}
    }
   i = 0
   if (trans) {
	 while(i < rvec.length) {
        	var j = 0; while(j < vec.length) {
              rvec(i) = logSumExp(rvec(i), (mat(j)(i) + vec(j) + lalpha))
               j += 1
        	}
        i += 1
      }
    } else {
      while(i < rvec.length) {
        var j = 0; 
        while(j < vec.length) {
          rvec(i) = logSumExp(rvec(i), (mat(i)(j) + vec(j) + lalpha))
          j += 1
        }
        i += 1
      }
    }
  }
  
  def computeScoresBackwards (inst_features_array: Seq[Seq[Feature]], takeExp: Boolean) = {
    setMatrix(ri)
    setTensor(mi)
    var d = 0; while (d < inst_features_array.length) {
      val inst_features = inst_features_array(d)
      var k = 0; while (k < inst_features.length) {
        val inst = inst_features(k)
        if (inst.prv < 0) {
          ri(d)(inst.cur) += lambdas(inst.fid)
        } else 
            mi(d)(inst.prv)(inst.cur) += lambdas(inst.fid)
        k += 1
      }
      k = 0
      d += 1
    }
    if (takeExp) {
      var d = 0; while (d < inst_features_array.length) {
        var k = 0; while (k < nls) {
          ri(d)(k) = exp(ri(d)(k))
          var c = 0; while (c < nls) {
            mi(d)(k)(c) = exp(mi(d)(k)(c));
            c += 1
          }
          k += 1
        }
        d += 1
      }
    }
  }
  
  override protected def backwardPass(iseq: Seq[AbstractInstance]) = {
    val len = iseq.length
    setArrayTo(beta(len),0.0) 
    var i = len - 1 // end of array,
    while (i >= 0) {
      val segsToConsider = (len - 1 - i) min segSize
      val inst_features_array : Seq[Seq[Feature]] = 
	Vector.tabulate(segsToConsider){j => 
	  val inst = iseq((i + j + 1))
	  val cvec = inst.getCompVec
          cvec(j)}
      computeScoresBackwards(inst_features_array,false)
      setArrayTo(beta(i),0.0)
      var d = 0
      while (d < segsToConsider) {    
        val ip = i + d + 1 // position of the end of the subsequent segment
        setArrayTo(t2, 0.0)
        Array.copy(beta(ip),0,tmp,0,tmp.length) // have to reset this for each segment
        assign1(tmp,ri(d),(_ + _)) 
        matrixMultLog(mi(d), tmp, t2, 1.0, 0.0, false)
        if (d > 0) assign1(beta(i), t2, (logSumExp _)) else assign1(beta(i), t2, (_ + _))
        d += 1
      } 
      i -= 1      
    }
  } 
}

abstract class DenseSemiCrf(nls: Int, nfs: Int, segSize: Int, gPrior: Double) extends DenseCrf(nls,nfs,segSize,gPrior) with SemiCrf {
  
  override def regularize() = {
    var i = 0
    var llMod = 0.0
    while (i < lambdas.length) {
      val li = lambdas(i)
      gradient(i) = li * invSigSqr
      featureExpectations(i) = 0.0 // need to set this to zero again
      llMod += (li * li * invSigSqr) / 2.0
      i += 1
    }
    llMod
  }
  
  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    setArrayTo(alpha(0), 0.0)
    var i = 1
    while (i <= iseq.length) {
      val inst = iseq(i-1)
      val label = inst.label
      val instFeatures = inst.getCompVec
      val curSegId = inst.segId
      val endOfSeg = ((i == iseq.length) || !(curSegId == iseq(i).segId))
      computeScores(instFeatures,false)
      setArrayTo(alpha(i),0.0)
      var d = 0
      val sl_l = beta(i-1)  // the beta vector is fixed over segment sizes for this computation
      while (d < (i min segSize)) {
        val ip = i - d - 1
        val riD = ri(d)
        val miD = mi(d)
        val instFeaturesD = instFeatures(d)
        val dSegId = iseq(ip).segId
        val prevSegId = if (ip > 0) iseq(ip - 1).segId else -1
        setArrayTo(tmp,-Double.MaxValue)
        matrixMultLog(miD,alpha(ip),tmp,1.0,0.0,true)
        assign1(tmp,riD,(_ + _))
        if (d > 0) assign1(alpha(i), tmp, logSumExp _) else assign1(alpha(i),tmp,(_ + _))
        var k = 0
        while (k < instFeaturesD.length) {
          val fk = instFeaturesD(k)
          if ((fk.cur == label) && ((segSize < 2) || (endOfSeg && (dSegId == curSegId) && (prevSegId == (curSegId - 1)))) &&
                ((fk.prv < 0) || ((ip > 0) && (iseq(ip - 1).label == fk.prv)))) {
                  gradient(fk.fid) -= fk.value
                  seqLogLi += lambdas(fk.fid)
                }
          if (fk.prv < 0) 
            featureExpectations(fk.fid) = logSumExp(featureExpectations(fk.fid), (tmp(fk.cur) + sl_l(fk.cur)))
          else
            featureExpectations(fk.fid) = logSumExp(featureExpectations(fk.fid), alpha(ip)(fk.prv) + riD(fk.cur) + miD(fk.prv)(fk.cur) + sl_l(fk.cur)) 
          k += 1
        }
        d += 1
      }
      i += 1
    }
    seqLogLi
  }
  
  override def gradOfSeq(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double = {
    reset(true,iseq.length) // full reset
    var xx = 0
    while (xx < nfs) { featureExpectations(xx) = -Double.MaxValue; xx += 1 }
    backwardPass(iseq)
    var sll = forwardPass(iseq)
    val zx = alpha(iseq.length).foldLeft(-Double.MaxValue){logSumExp _}
    sll -= zx
    var i = 0
    while (i < nfs) {gradient(i) += exp(featureExpectations(i) - zx); i += 1}
    sll
  }

}


abstract class StochasticSemiCrf(nls: Int, nfs: Int, segSize: Int, opts: Options) extends StochasticCrf(nls,nfs,segSize,opts) with SemiCrf {

  override def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    setArrayTo(alpha(0), 0.0)
    var i = 1
    while (i <= iseq.length) {
      val inst = iseq(i-1)
      val label = inst.label
      val instFeatures = inst.getCompVec
      val curSegId = inst.segId
      val endOfSeg = ((i == iseq.length) || !(curSegId == iseq(i).segId))
      computeScores(instFeatures,false)
      setArrayTo(alpha(i),0.0)
      var d = 0
      val sl_l = beta(i-1)  // the beta vector is fixed over segment sizes for this computation
      while (d < (i min segSize)) {
        val ip = i - d - 1
        val riD = ri(d)
        val miD = mi(d)
        val instFeaturesD = instFeatures(d)
        val dSegId = iseq(ip).segId
        val prevSegId = if (ip > 0) iseq(ip - 1).segId else -1
        setArrayTo(tmp,-Double.MaxValue)
        matrixMultLog(miD,alpha(ip),tmp,1.0,0.0,true)
        assign1(tmp,riD,(_ + _))
        if (d > 0) assign1(alpha(i), tmp, logSumExp _) else assign1(alpha(i),tmp,(_ + _))
        var k = 0
        while (k < instFeaturesD.length) {
          val fk = instFeaturesD(k)
          val gref = gradient.get(fk.fid) match {case Some(v) => v case None => val nv = new DoubleCell(0.0,0.0); gradient += (fk.fid -> nv); nv}
          if ((fk.cur == label) && ((segSize < 2) || (endOfSeg && (dSegId == curSegId) && (prevSegId == (curSegId - 1)))) &&
                ((fk.prv < 0) || ((ip > 0) && (iseq(ip - 1).label == fk.prv)))) {
        	 gref.g_=(gref.g + fk.value)
                  seqLogLi += lambdas(fk.fid) * fk.value
                }
          if (fk.prv < 0) 
            gref.e_=(logSumExp(gref.e, (tmp(fk.cur) + sl_l(fk.cur))))
          else
            gref.e_=(logSumExp(gref.e, alpha(ip)(fk.prv) + riD(fk.cur) + miD(fk.prv)(fk.cur) + sl_l(fk.cur))) 
          k += 1
        }
        d += 1
      }
      i += 1
    }
    seqLogLi
  }

  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = {
    val asize = batchSize min seqAccessor.length
    var gradNormalizer = 0.0
    for (i <- curPos until curPos + asize) {
      val j = i % seqAccessor.length
      val iseq = seqAccessor(j)
      if (iseq.length > 0) {
        reset(iseq.length)
        gradient.foreach {case (k,v) => v.e_=(0.0)} // reset expectations to zero
        backwardPass(iseq)
        forwardPass(iseq)
	val pzx = alpha(iseq.length).foldLeft(-Double.MaxValue){logSumExp _}
        val zx_p = if (pzx < Double.MaxValue) pzx else Double.MaxValue
        val zx = if ((math.abs(zx_p) < 1.0E-100)) 1.0E-100 else zx_p
        for ((k,cell) <- gradient) {
          cell.g_= (cell.g - (exp(cell.e - zx)))
          val cabs = math.abs(cell.g)
          if (cabs > gradNormalizer) { gradNormalizer = cabs }
        }
      }
    }
    curPos += asize
    // normalization here will prevent gradient components from having a value greater than 100.0
    // Such values in the gradient are problematic in subsequent numerical calculations
    if (gradNormalizer > 100.0) {
    	val nn = 100.0 / gradNormalizer
    	for ((k,cell) <- gradient) cell.g_= ((cell.g * nn)  - lambdas(k) * invSigSqr)
    } 
    //for ((k,cell) <- gradient) cell.g_= (cell.g  - lambdas(k) * invSigSqr) } 
    None
  }

  override protected def reset(l: Int) : Unit = reset(true,l)
  
}
