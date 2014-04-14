package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{AbstractInstance, PsaLearner, CompactFeature}
import org.mitre.jcarafe.util.{SparseVector, SparseVectorAsMap}
import java.io.File
import collection.mutable.HashMap
import cern.colt.map.OpenIntDoubleHashMap
/*
 * This version of a maxent object is 'stateless' in the sense that it doesn't store the gradient/likelihood within the 
 * object but exposes it such that other optimizers or distributed computing algorithms can use it as a sub-routine
 */
class SparseStatelessMaxEnt(val nls: Int, val nfs: Int) extends MaxEntCore with Serializable {
  
  class DoubleCell(var g: Double, var e: Double)
  
  val predNFS = nfs / nls
  
  def getSimpleGradientCompact(gr: Map[Int,DoubleCell], inv: Boolean = true) : SparseVector = {
    val nm = gr map {case (k,v) => if (inv) (k,(v.e - v.g)) else (k,(v.g - v.e))}
    SparseVector(nm)
  }
  
/*
  def getSimpleGradient(gr: Map[Int,DoubleCell], inv: Boolean = true) : Map[Int,Double] = {
    gr map {case (k,v) => if (inv) (k,(v.e - v.g)) else (k,(v.g - v.e))}
  }
*/
  def getSimpleGradient(gr: Map[Int,DoubleCell], inv: Boolean = true) : SparseVectorAsMap = {
    val mn = new OpenIntDoubleHashMap
    var s = 0
    gr foreach {case (k,v) => 
      s += 1;
      if (inv) mn.put(k, (v.e - v.g)) 
      else mn.put(k,(v.g - v.e))}
    new SparseVectorAsMap(s, mn)
  }
  
  def gradientOfSingleElement(el: AbstractInstance, lambdas: Array[Double], inv: Boolean = true) : (Double, SparseVectorAsMap) = {
    val localGrad : collection.mutable.Map[Int,Double] = HashMap[Int,Double]()
    var gr: Map[Int, DoubleCell] = Map[Int, DoubleCell]()
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
      val loss = if (inv) -(cost * w) else cost * w
      (loss, getSimpleGradient(gr, inv))
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
      val loss = if (inv) -(math.log(scores(trueLabel))) else (math.log(scores(trueLabel))) 
      (loss, getSimpleGradient(gr,inv))
    }
  }
}

object TestGradients {
  
  def main(args: Array[String]) = {
    val sgen = new MaxEntTrainingSeqGen
    
    val instances = sgen.toAbstractInstanceSeq(sgen.deserializeFromFile(args(0)), true)
    val sp = new SparseStatelessMaxEnt(sgen.getNumberOfStates, sgen.getNumberOfFeatures)
    val lambdas = Array.fill(sgen.getNumberOfFeatures)(0.0)
    val denseGrad = Array.fill(sgen.getNumberOfFeatures)(0.0)
    var ind = 0
    instances foreach {i =>
      val fs = i.getCompactVec
      fs foreach {s => print(" " + s.fid)}
      println
      val (ll,g) = sp.gradientOfSingleElement(i, lambdas, true)
      println("Grad for inst: " + ind)
      for (i <- 0 until sgen.getNumberOfFeatures) {
        if (g.umap.containsKey(i)) {
          print(" " + i + " => " + g.umap.get(i))
          denseGrad(i) += g.umap.get(i)
        } else {
          
        }
      }
      ind += 1
      println
    }
    println("Total gradient: ")
    denseGrad foreach {v => print(" " + v)}
    println
    ind = 0
    val control = new SparseMaxEnt(sgen.getNumberOfStates, sgen.getNumberOfFeatures, new MEOptions) with PsaLearner[AbstractInstance]
    val cgrad = Array.fill(sgen.getNumberOfFeatures)(0.0)
    instances foreach {inst =>
      val loss = control.gradOfElement(inst)
      println("Control grad for " + ind)
      for (i <- 0 until sgen.getNumberOfFeatures) {
        val cv = control.gradient.get(i)        
        cv match {case Some(cell) =>
          cgrad(i) += (cell.g - cell.e)
          println("Grad component("+i+") => " + (cell.g - cell.e)) case None =>}         
        control.gradient.clear
      }
      println
      ind += 1
      }
    
    println("Cgrad 1")
    cgrad foreach {v => print(" " + v)}
    println
    val seqAccessor = new org.mitre.jcarafe.crf.MemoryAccessSeq(Vector(new org.mitre.jcarafe.crf.MemoryInstanceSequence(instances)))
    
    
    val gg = control.getGradient(false, seqAccessor)
    println("CONTROL GRAD:")
    val cgrad1 = Array.fill(sgen.getNumberOfFeatures)(0.0)
    control.gradient foreach {case (k,v) => cgrad(k) += v.g}
    cgrad foreach {g => print(" " + g)}
    println
    
    val denseControl = new MaxEnt(sgen.getNumberOfStates, sgen.getNumberOfFeatures, 1E300) with org.mitre.jcarafe.crf.CondLogLikelihoodLearner[AbstractInstance]
    val denseG = denseControl.getGradient(false, seqAccessor)
    println("DENSE GRADIENT:")
    denseControl.gradient foreach {g => print(" " + g)}
    println
  }
}