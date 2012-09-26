package org.mitre.jcarafe.crf

import scala.math.exp

import org.mitre.jcarafe.util.Options

abstract class GeneralizedEMCrf(nls: Int, nfs: Int, segSize: Int, opts: Options) extends StochasticCrf(nls,nfs,segSize,opts) {
	
  /**
   * Constrained Beta values. Need values for each segment length for each label (in general, Semi-CRF case)  
   */
  var conBeta: Matrix = Array.fill(1,nls)(0.0)
  
  /**
   * Current constrained alpha values used for Forward-Backward computation
   */
  var conCurA: Array[Double] = Array.fill(nls)(0.0)
  
  /**
   * Alpha values at the next position used for Forward-Backward computation
   */
  var conNewA: Array[Double] = Array.fill(nls)(0.0)
  
  var conTmp: Array[Double] = Array.fill(nls)(0.0)
  
  var conScale: Array[Double] = Array.fill(1)(0.0)
  
  override protected def reset(all: Boolean, slen: Int) = {
    super.reset(all,slen)
    val aNls = if (adjustible) slen else nls
    if (conBeta.length < slen) {
      conBeta = Array.fill(2*slen,aNls)(0.0)
      conScale = Array.fill(2*slen)(0.0)      
    }	
  }
  // run backward pass twice - once constrained and once not
  
  def backwardPassConstrained(iseq:Seq[AbstractInstance]) = {
    val len = iseq.length
    scale(len-1) = curNls
    assign(conBeta(len-1),((_:Double) => 1 / scale(len-1)))
    var i = len - 1
    while (i > 0) {
      computeScoresConstrained(iseq,i,true)
      Array.copy(conBeta(i), 0, tmp, 0, curNls)
      assign1(tmp, ri(0), (_ * _))
      Crf.matrixMult(mi(0), tmp, conBeta(i-1), 1.0, 0.0, false)
      conScale(i-1) = vecSum(conBeta(i-1))
      assign(conBeta(i-1),(_ / conScale(i-1)))
      i -= 1
    }
  }
  
  override def forwardPass(iseq:Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      computeScoresConstrained(iseq, i, true)
      Array.copy(conCurA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, conNewA, 1.0, 0.0, true)
      assign1(conNewA, ri(0), (_ * _))
      var k = 0
      while (k < instFeatures(0).length) {
        val inst = instFeatures(0)(k)
        val gref = gradient.get(inst.fid) match {
          case Some(v) => v 
          case None => 
            val nv = new DoubleCell(0.0,0.0)
            gradient += ((inst.fid,nv))
            nv}
        if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
          gref.g_= (gref.g + inst.value)
          seqLogLi += lambdas(inst.fid) * inst.value
        } else if (label < 0) {
        	val ex = (conNewA(inst.cur) * conBeta(i)(inst.cur))
        	gref.g_=(gref.g + inst.value * ex)
        	seqLogLi += lambdas(inst.fid) * ex * inst.value
        }
        if (inst.prv < 0) gref.e_= (gref.e + newA(inst.cur) * beta(i)(inst.cur))
        else gref.e_= (gref.e + curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur))
        k += 1
      }
      Array.copy(newA,0,curA,0,curNls)
      Array.copy(conNewA,0,conCurA,0,curNls)
      assign(curA,(_ / scale(i)))
      assign(conCurA,(_ / conScale(i)))
      i += 1
    }
    seqLogLi
  }
  
  def computeScoresConstrained (absInstSeq: Seq[AbstractInstance], pos: Int, takeExp: Boolean) = {
    val inst_features = absInstSeq(pos).getCompVec
    val curLabel = absInstSeq(pos).label
    val prevLabel = if (pos > 0) absInstSeq(pos-1).label else -1
    Crf.setMatrix(ri)
    Crf.setTensor(mi)
    var d = 0; while (d < inst_features.length) {
      var k = 0; while (k < inst_features(d).length) {
        val inst = inst_features(d)(k)
        if (inst.prv < 0) {
          if ((inst.cur == curLabel) || (curLabel < 0)) ri(d)(inst.cur) += lambdas(inst.fid) * inst.value else ri(d)(inst.cur)
        } else 
            if (((inst.cur == curLabel) || (curLabel < 0)) && ((inst.prv == prevLabel) || (prevLabel < 0))) mi(d)(inst.prv)(inst.cur) += lambdas(inst.fid) * inst.value
        k += 1
      }
      k = 0
      d += 1
    }
    // add constraints so that states and transitions incompatable with provided labels have a score of negative infinity
    d = 0; while (d < inst_features.length) {
    	var k = 0; while (k < nls) {
    		if ((curLabel >= 0) && (k != curLabel)) ri(d)(k) = -(Double.MaxValue)
    		var c = 0; while ( c < nls) {
    			if ((curLabel >= 0) && (prevLabel >= 0)) {
    				if ((k != curLabel) || (c != prevLabel)) mi(d)(c)(k) = -(Double.MaxValue)
    			} else if ((curLabel < 0) && (prevLabel >= 0)) {
    				if (c != prevLabel) mi(d)(c)(k) = -(Double.MaxValue)
    			} else if ((prevLabel < 0) && (curLabel >= 0)) {
    				if (k != curLabel) mi(d)(c)(k) = -(Double.MaxValue)
    			}
    		}
    	}
    }
    if (takeExp) {
      var d = 0; while (d < inst_features.length) {
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
    
}
