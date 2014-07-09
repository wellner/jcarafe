package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.{ Options, SparseVectorAsMap }

import cern.colt.map.OpenIntDoubleHashMap

abstract class KLDivMinimizingCrf(lambdas: Array[Double], nls: Int, nfs: Int, segSize: Int, gPrior: Double, nNfs: Int = 0, nGates: Int = 0) 
extends DenseCrf(lambdas, nls, nfs, segSize, gPrior, nNfs, nGates) {
  
  def this(nls: Int, nfs: Int, ss: Int, gPrior: Double) = this(Array.fill(nfs)(0.0),nls,nfs,ss,gPrior)
  
  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val abstractInst = iseq(i)
      val instFeatures = abstractInst.getCompVec
      val label = abstractInst.label
      
      computeScores(instFeatures, true)
      Array.copy(curA, 0, tmp, 0, curNls)
      matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
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
          featureExpectations(inst.fid) += newA(inst.cur) * beta(i)(inst.cur) * inst.value          
        } else if (i > 0) {
          val prevInst = iseq(i-1)
          val pairMarginal = abstractInst.conditionalProb(inst.cur) * prevInst.conditionalProb(inst.prv) 
          gradient(inst.fid) -= inst.value * pairMarginal
          seqLogLi += lambdas(inst.fid) * inst.value * pairMarginal
          featureExpectations(inst.fid) += curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value
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

class SparseStatelessKLDivMinimizingCrf(
    nls: Int,
    nfs: Int,
    segSize: Int,
    opts: Options,
    nNfs: Int = 0,
    nGates: Int = 0
    ) extends KLDivMinimizingStochasticCrf(nls, nfs, segSize, opts, nNfs, nGates) with Serializable {

  var localParams: Array[Double] = Array[Double]() // unfortunate way to have to do this
  override def getLambdas = localParams
  private var gradNormalizer = 0.0
  
  def train(accessSeq: AccessSeq[AbstractInstance], max_iters: Int, modelIterFn: Option[(CoreModel, Int) => Unit] = None): CoreModel = {
    new CoreModel(getLambdas, nls, nfs)
  }
  
  def getSimpleGradient(gr: collection.mutable.Map[Int, DoubleCell], inv: Boolean = true, gboundary: Boolean = false): SparseVectorAsMap = {
    val mn = new OpenIntDoubleHashMap
    var s = 0
    val params = getLambdas
    gr foreach {
      case (k, v) =>
        s += 1;
        val gComp = if (inv) (v.e - v.g) else (v.g - v.e)
        // enforce upper and lower limits on gComp to avoid very large parameter updates 
        val aComp = if (gboundary) (if (gComp > 100.0) 100.0 else if (gComp < -100.0) -100.0 else gComp) else gComp
        mn.put(k, aComp)
    }
    new SparseVectorAsMap(s, mn)
  }

  def getGradientSingleSequence(s: InstanceSequence, curLambdas: Array[Double], inv: Boolean = true, gboundary: Boolean = false): (Double, SparseVectorAsMap) = {
    localParams = curLambdas // set the parameters to those passed in via curLambdas
    val iseq = s.iseq
    val sl = iseq.length
    var ll = 0.0
    val params = getLambdas
    gradient.clear // clear the 
    if (sl > 0) {
      reset(iseq.length)
      backwardPass(iseq)
      ll = forwardPass(iseq)
      val pzx = vecSum(curA)
      val zx = if (pzx < Double.MaxValue) pzx else Double.MaxValue
      ll -= math.log(zx)
      for (k <- 0 until iseq.length) ll -= math.log(scale(k))
      for ((k, cell) <- gradient) {
        cell.e_=(cell.e / zx) // normalize expectation and hold in expectation cell - will take difference with constraints in getSimpleGradient method          
      }
    }
    (-ll, getSimpleGradient(gradient, inv, gboundary)) // get negative LL and inverted gradient for LBFGS optimization
  }
}

abstract class KLDivMinimizingStochasticCrf(
    nls: Int,
    nfs: Int,
    segSize: Int,
    opts: Options,
    nNfs: Int = 0,
    nGates: Int = 0
    ) extends StochasticCrf(nls, nfs, segSize, opts, nNfs, nGates) {
  
  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val abstractInst = iseq(i)
      val instFeatures = abstractInst.getCompVec
      val label = iseq(i).label
      computeScores(instFeatures, true)
      Array.copy(curA, 0, tmp, 0, curNls)
      matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val nfeas = instFeatures(0).length
      val instFeatures0 = instFeatures(0)
      while (k < nfeas) {
        val inst = instFeatures0(k)
        val gref = gradient.get(inst.fid) match {
          case Some(v) => v
          case None =>
            val nv = new DoubleCell(0.0, 0.0)
            gradient.update(inst.fid, nv)
            nv
        }
        if (inst.prv < 0) {
          val empiricalMarginal = abstractInst.conditionalProb(inst.cur)
          gref.g_=(gref.g + inst.value * empiricalMarginal)
          seqLogLi += lambdas(inst.fid) * inst.value * empiricalMarginal
          gref.e_=((gref.e + newA(inst.cur) * beta(i)(inst.cur)) * inst.value)
        } else {
          val prevInst = iseq(i-1)
          val pairMarginal = abstractInst.conditionalProb(inst.cur) * prevInst.conditionalProb(inst.prv)
          gref.g_=(gref.g + inst.value * pairMarginal)
          seqLogLi += lambdas(inst.fid) * inst.value * pairMarginal
          gref.e_=((gref.e + curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur)) * inst.value)
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