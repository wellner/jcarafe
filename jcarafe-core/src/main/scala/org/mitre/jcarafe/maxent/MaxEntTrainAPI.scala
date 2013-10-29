package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{AbstractInstance, PsaLearner, CompactFeature}
import java.io.File
import collection.mutable.HashMap

class MaxEntTrainAPI(f: File) {
  
  val eta_0 = 0.1
  var eta_t = eta_0
  val p_alpha = 0.99
  var t = 1
  val opts = new MEOptions    
  val sgen = new MaxEntTrainingSeqGen(opts)
  val ss = 
      if (f.isDirectory) {
          f.listFiles.toSeq flatMap
          { f: File => sgen.toAbstractInstanceSeq(sgen.deserializeFromFile(f), true) }        
      } else sgen.toAbstractInstanceSeq(sgen.deserializeFromFile(f), true)    
  val tr = new SparseMaxEntStateless(sgen.getNumberOfStates,sgen.getNumberOfFeatures,opts) with PsaLearner[AbstractInstance]
  val N = ss.length

  def getAbstractInstances(f: File) : Seq[AbstractInstance] = ss  
  
  def getGradient(ai: AbstractInstance) = {
    tr.gradientOfSingleElement(ai)
  }
    
  def update(u: collection.mutable.Map[Int,Double]) = {
    eta_t += eta_0 * scala.math.pow(p_alpha, (t / N))
    u foreach {case (k,v) => tr.lambdas(k) += v * eta_t}
  }
}

abstract class SparseMaxEntStateless(nls: Int, nfs: Int, opts: MEOptions) extends SparseMaxEnt(nls,nfs,opts) {
  
  def getSimpleGradient(gr: collection.mutable.Map[Int,DoubleCell]) : collection.mutable.Map[Int,Double] = {
    gr map {case (k,cell) => (k,cell.g - cell.e) }
  }
  
  def gradientOfSingleElement(el: AbstractInstance) : (Double, collection.mutable.Map[Int,Double])= {
    val localGrad : collection.mutable.Map[Int,Double] = HashMap[Int,Double]()
    val gr: collection.mutable.Map[Int, DoubleCell] = new HashMap[Int, DoubleCell]()
    val instFeatures: Array[CompactFeature] = el.getCompactVec
    val trueLabel = el.label
    val scores = classScoresNormalized(nls, predNFS, lambdas, instFeatures).toArray
    val w = el.instWeight
    var k = 0
    val il = instFeatures.length
    if (el.hasPosterior) {
      while (k < il) {
        var l = 0
        val inst = instFeatures(k)
        val fid = inst.fid
        while (l < nls) {
          val offset = l * predNFS
          val actualIndex = fid + offset
          val v = inst(l)
          val pMass = el.conditionalProb(l)
          val gref = gr.get(actualIndex) match {
            case Some(v) => v
            case None =>
              val nv = new DoubleCell(0.0, 0.0)
              gr += ((actualIndex, nv))
              nv
          }
          gref.g_=(gref.g + v * w * pMass) // constraint based on posterior probability mass
          gref.e_=(gref.e + scores(l) * v * w)
          l += 1
        }
        k += 1
      }
      var cost = 0.0
      k = 0
      while (k < el.getRange) {
        cost += math.log(scores(k)) * el.conditionalProb(k)
        k += 1
      }
      (cost * w, getSimpleGradient(gr))
    } else {
      while (k < il) {
        var l = 0
        val inst = instFeatures(k)
        val fid = inst.fid
        while (l < nls) {
          val offset = l * predNFS
          //val v = inst(l)
          val v = inst.v
          val actualIndex = fid + offset
          val gref = gr.get(actualIndex) match {
            case Some(v) => v
            case None =>
              val nv = new DoubleCell(0.0, 0.0)
              gr += ((actualIndex, nv))
              nv
          }
          if (l == trueLabel) {
            gref.g_=(gref.g + v)
          }
          gref.e_=(gref.e + scores(l) * v)
          l += 1
        }
        k += 1
      }
      (math.log(scores(trueLabel)), getSimpleGradient(gr))
    }
  }
}