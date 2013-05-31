package org.mitre.jcarafe.crf

abstract class KLDivMinimizingCrf(lambdas: Array[Double], nls: Int, nfs: Int, segSize: Int, gPrior: Double, nNfs: Int = 0, nGates: Int = 0) 
extends DenseCrf(lambdas, nls, nfs, segSize, gPrior, nNfs, nGates) {
  
  def this(nls: Int, nfs: Int, ss: Int, gPrior: Double) = this(Array.fill(nfs)(0.0),nls,nfs,ss,gPrior)
  
  override protected def forwardPass(iseq: Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val abstractInst = iseq(i)
      val instFeatures = abstractInst.getCompVec
      val label = abstractInst.label
      computeScores(instFeatures, true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val instFeatures0 = instFeatures(0)
      val nfeas = instFeatures0.length
      while (k < nfeas) {
        val inst = instFeatures0(k)
         
        if (inst.prv < 0) {
          val empiricalMarginal = abstractInst.conditionalProb(inst.cur)
          gradient(inst.fid) -= inst.value * empiricalMarginal          
          seqLogLi += lambdas(inst.fid) * inst.value * empiricalMarginal
          featureExpectations(inst.fid) += newA(inst.cur) * beta(i)(inst.cur) * inst.value * empiricalMarginal
        } else if (i > 0) {
          val prevInst = iseq(i-1)
          val pairMarginal = abstractInst.conditionalProb(inst.cur) * prevInst.conditionalProb(inst.prv) 
          gradient(inst.fid) -= inst.value * pairMarginal
          seqLogLi += lambdas(inst.fid) * inst.value * pairMarginal
          featureExpectations(inst.fid) += curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value * pairMarginal
        }
        k += 1
      }
      Array.copy(newA, 0, curA, 0, curNls)
      assign(curA, (_ / scale(i)))
      i += 1
    }
    seqLogLi
  }

  
  
}