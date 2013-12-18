package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.Options

trait KLDivGEMCrf extends GeneralizedEMCrf {
  override protected def updateScoreMatrices(iseq: Seq[AbstractInstance], pos: Int) = {    
    val instEl = iseq(pos)
    val instFeatures = iseq(pos).getCompVec
    val curLabel = iseq(pos).label
    val prevLabel = if (pos > 0) iseq(pos-1).label else -1
    var d = 0; while (d < instFeatures.length) {
      var k = 0; while (k < instFeatures(d).length) {
        val inst = instFeatures(d)(k)
        if (inst.prv < 0) {
          if ((inst.cur == curLabel) || (curLabel < 0)) {
            val empiricalMarginal = instEl.conditionalProb(inst.cur)
            conRi(d)(inst.cur) += lambdas(inst.fid) * inst.value * empiricalMarginal
          }
        } else 
            if ((pos > 0) && ((inst.cur == curLabel) || (curLabel < 0)) && ((inst.prv == prevLabel) || (prevLabel < 0))) {
              val prevInst = iseq(pos-1)
              val pairMarginal = instEl.conditionalProb(inst.cur) * prevInst.conditionalProb(inst.prv)
              conMi(d)(inst.prv)(inst.cur) += lambdas(inst.fid) * inst.value * pairMarginal
            }               
        k += 1
      }
      d += 1
    }    
  }  
}

abstract class StochasticKLDivMinimizingGEMCrf(nls: Int, nfs: Int, segSize: Int, opts: Options) extends StochasticGeneralizedEMCrf(nls, nfs, segSize, opts) with KLDivGEMCrf

abstract class DenseKLDivMinimizingGEMCrf(nls: Int, nfs: Int, segSize: Int, opts: Options) extends DenseGeneralizedEMCrf(nls, nfs, segSize, opts) with KLDivGEMCrf
