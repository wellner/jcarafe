package org.mitre.jcarafe.crf

import scala.math.exp
import org.mitre.jcarafe.util.Options

abstract class DenseCRFConfidences(model: CoreModel, val testLL: Boolean = false) extends DenseCrf(model) {

  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val abstEl = iseq(i)
      val instFeatures = abstEl.getCompVec
      val label = if (testLL) abstEl.orig else abstEl.label
      val prevLabel = if (i > 0) { if (testLL) iseq(i-1).orig else iseq(i-1).label } else -1
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val instFeatures0 = instFeatures(0)
      val nfeas = instFeatures0.length
      while (k < nfeas) {
        val inst = instFeatures0(k)
        if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (prevLabel == inst.prv)))) {
          gradient(inst.fid) -= inst.value
          seqLogLi += lambdas(inst.fid) * inst.value
        }
        if (inst.prv < 0) featureExpectations(inst.fid) += newA(inst.cur) * beta(i)(inst.cur) * inst.value
        else featureExpectations(inst.fid) += curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value
        k += 1
      }
      val unNormScores : IndexedSeq[Double] = for (l <- 0 until curNls) yield newA(l)*beta(i)(l)
      val total = unNormScores.foldLeft(0.0){ _ + _ }
      for (l <- 0 until curNls) {
    	abstEl.setConditionalProb(l,unNormScores(l) / total)
      }
      Array.copy(newA,0,curA,0,curNls)
      assign(curA,(_ / scale(i)))
      i += 1
    }
    seqLogLi
  }  

  override def gradOfSeq(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double = {
    reset(false,iseq.length)
    var xx = 0
    while (xx < nfs) { featureExpectations(xx) = 0.0; xx += 1 }
    backwardPass(iseq)
    var sll = forwardPass(iseq)
    val zx = vecSum(curA) // curA will be set to the last position within forwardPass
    sll -= math.log(zx)
    for (k <- 0 until iseq.length) sll -= math.log(scale(k))
    var i = 0
    while (i < nfs) {
      gradient(i) += featureExpectations(i) / zx
      i += 1
    }
    sll
  }
  

}
