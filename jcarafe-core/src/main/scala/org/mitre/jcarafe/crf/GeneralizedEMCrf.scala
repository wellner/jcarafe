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
  
  /**
   * For each segment size (general case) the ri matrix holds state scores for each label
   */
  var conRi: Matrix = Array.fill(segSize, nls)(0.0)
  /**
   * For each segment size, the mi matrix holds transition scores for adjacent labels
   */
  var conMi: Tensor = Array.fill(segSize, nls, nls)(0.0)
  
  val conMarginals : Matrix = Array.fill(nls,nls)(1.0 / (nls * nls))
  
  val conMarginalState : Array[Double] = Array.fill(nls)(1.0 / nls)
  
  def printMi(m: Array[Array[Array[Double]]]) = {
    m(0) foreach {
      row =>
        row foreach {el => print(" " + el)}
        println
    }
    println
  }
  
  
  override protected def reset(all: Boolean, slen: Int) = {
    super.reset(all,slen)
    val aNls = if (adjustible) slen else nls
    if (conBeta.length < slen) {
      conBeta = Array.fill(2*slen,aNls)(0.0)
      conScale = Array.fill(2*slen)(0.0)      
    }
    for (i <- 0 until aNls) conCurA(i) = 1.0 // need to initialize alphas properly
  }
  
  // run backward pass twice - once constrained and once not
  def backwardPassConstrained(iseq:Seq[AbstractInstance]) = {
    super.backwardPass(iseq) 
    val len = iseq.length
    conScale(len-1) = curNls
    assign(conBeta(len-1),((_:Double) => 1 / conScale(len-1)))
    var i = len - 1
    while (i > 0) {
      computeScoresConstrained(iseq,i,true)
      Array.copy(conBeta(i), 0, tmp, 0, curNls)
      assign1(tmp, conRi(0), (_ * _))
      Crf.matrixMult(conMi(0), tmp, conBeta(i-1), 1.0, 0.0, false)
      conScale(i-1) = vecSum(conBeta(i-1))
      assign(conBeta(i-1),(_ / conScale(i-1)))
      i -= 1
    }
  }
  
  override def backwardPass(iseq: Seq[AbstractInstance]) = backwardPassConstrained(iseq)
  
  def setConstrainedMarginals(ri: Array[Double], mi: Matrix, pos: Int) = {
    var i = 0
    var j = 0
    while (i < nls) {
      j = 0; while (j < nls) {
        conMarginals(i)(j) = 0.0
        j += 1
      }  
      conMarginalState(i) = 0.0
      i += 1
    }
    var tot = 0.0
    i = 0; while (i < nls) {
      j = 0; while (j < nls) {
        val u = conCurA(i) * ri(j) * mi(i)(j) * conBeta(pos)(j)
        conMarginals(i)(j) = u         
        tot += u
        j += 1
      }
      i += 1
    }
    var stTot = 0.0
    i = 0; while (i < nls) {
      val u = conNewA(i) * conBeta(pos)(i)
      conMarginalState(i) = u
      stTot += u
      i += 1
    }
    i = 0; while (i < nls) {
      conMarginalState(i) /= stTot
      i += 1
    }
    i = 0; while (i < nls) {
      j = 0; while (j < nls) {
        conMarginals(i)(j) /= tot
        j += 1
      }
      i += 1
    }
    /*
    println("Marginals: ")
    conMarginalState foreach {i => print(" " + i)}
    println
    */
  }
  
  override def forwardPass(iseq:Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScoresConstrained(iseq, i, true)
      Array.copy(conCurA, 0, tmp, 0, curNls)
      Crf.matrixMult(conMi(0), tmp, conNewA, 1.0, 0.0, true)
      assign1(conNewA, conRi(0), (_ * _))
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      // compute actual marginals over constrained sequence
      // these will be used to set the "empirical" feature counts
      setConstrainedMarginals(conRi(0), conMi(0), i) 

      var k = 0
      while (k < instFeatures(0).length) {
        val inst = instFeatures(0)(k)
        val gref = gradient.get(inst.fid) match {
          case Some(v) => v 
          case None => 
            val nv = new DoubleCell(0.0,0.0)
            gradient += ((inst.fid,nv))
            nv}
        if (inst.prv < 0) {
          gref.e_= (gref.e + newA(inst.cur) * beta(i)(inst.cur) * inst.value)
          val gSc = conMarginalState(inst.cur)
          gref.g_= (gref.g + gSc)
          seqLogLi += lambdas(inst.fid) * gSc 
        } 
        else {
          gref.e_= (gref.e + curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value)
          val gSc = conMarginals(inst.prv)(inst.cur)
          gref.g_= (gref.g + gSc)
        }
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
  
  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = {
    val asize = batchSize min seqAccessor.length
    var gradNormalizer = 0.0
    var totalLL = 0.0
    for (i <- curPos until curPos + asize) {
      val j = i % seqAccessor.length
      val iseq = seqAccessor(j)
      val sl = iseq.length
      if (sl > 0) {
        reset(iseq.length)
        gradient.foreach { case (k, v) => v.e_=(0.0) } // reset expectations to zero
        backwardPass(iseq)
        var sll = forwardPass(iseq)
        val pzx = vecSum(curA)
        val gzx = vecSum(conCurA)
        val zx = if (pzx < Double.MaxValue) pzx else Double.MaxValue
        sll -= math.log(zx)
        for (k <- 0 until iseq.length) sll -= math.log(scale(k))
        for ((k, cell) <- gradient) {
          cell.g_=((cell.g / gzx) - (cell.e / zx))
          val cabs = math.abs(cell.g)
          if (cabs > gradNormalizer) { gradNormalizer = cabs }
        }
        totalLL -= sll
      }
    }
    curPos += asize
    // normalization here will prevent gradient components from having a value greater than 100.0
    // Such values in the gradient are problematic in subsequent numerical calculations
    if (gradNormalizer > 50.0) {
      numGradIssues += 1
      val nn = 50.0 / gradNormalizer
      for ((k, cell) <- gradient) cell.g_=((cell.g * nn) - lambdas(k) * invSigSqr)
    } else {
      for ((k, cell) <- gradient) {
        cell.g_=(cell.g - lambdas(k) * invSigSqr)
      }
    }
    Some(totalLL)
  }
  
  def computeScoresConstrained (absInstSeq: Seq[AbstractInstance], pos: Int, takeExp: Boolean) = {
    val inst_features = absInstSeq(pos).getCompVec
    val curLabel = absInstSeq(pos).label
    val prevLabel = if (pos > 0) absInstSeq(pos-1).label else -1
    Crf.setMatrix(conRi)
    Crf.setTensor(conMi)
    var d = 0; while (d < inst_features.length) {
      var k = 0; while (k < inst_features(d).length) {
        val inst = inst_features(d)(k)
        if (inst.prv < 0) {
          if ((inst.cur == curLabel) || (curLabel < 0)) conRi(d)(inst.cur) += lambdas(inst.fid) * inst.value else conRi(d)(inst.cur)
        } else 
            if (((inst.cur == curLabel) || (curLabel < 0)) && ((inst.prv == prevLabel) || (prevLabel < 0))) 
              conMi(d)(inst.prv)(inst.cur) += lambdas(inst.fid) * inst.value
        k += 1
      }
      d += 1
    }
    // add constraints so that states and transitions incompatable with provided labels have a score of negative infinity
    d = 0; while (d < inst_features.length) {
    	var k = 0; while (k < nls) {
    		if ((curLabel >= 0) && (k != curLabel)) conRi(d)(k) = -(Double.MaxValue)
    		var c = 0; while ( c < nls) {
    			if ((curLabel >= 0) && (prevLabel >= 0)) {
    				if ((k != curLabel) || (c != prevLabel)) conMi(d)(c)(k) = -(Double.MaxValue)
    			} else if ((curLabel < 0) && (prevLabel >= 0)) {
    				if (c != prevLabel) conMi(d)(c)(k) = -(Double.MaxValue)
    			} else if ((prevLabel < 0) && (curLabel >= 0)) {
    				if (k != curLabel) conMi(d)(c)(k) = -(Double.MaxValue)
    			}
    			c += 1
    		}
    		k += 1
    	}
    	d += 1
    }
    if (takeExp) {
      var d = 0; while (d < inst_features.length) {
        var k = 0; while (k < nls) {
          conRi(d)(k) = exp(conRi(d)(k))
          var c = 0; while (c < nls) {
            conMi(d)(k)(c) = exp(conMi(d)(k)(c));
            c += 1
          }
          k += 1
        }
        d += 1
      }
    }
  }
    
}
