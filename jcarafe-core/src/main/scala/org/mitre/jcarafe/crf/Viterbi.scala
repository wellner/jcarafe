/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

object Viterbi {
  def apply(dynamic: Boolean, ss: Int, core: CoreModel, post: Boolean = false) = 
    if (core.nGates > 0) new NeuralViterbi(dynamic, ss, core)
    else new Viterbi(dynamic, ss, core, post)
}

abstract class DecodingAlgorithm extends Serializable {
  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double
  def getCopyOf : DecodingAlgorithm
  def assignBestSequence(instSeq: InstanceSequence) : Unit = {
    val sp = assignBestSequence(instSeq.iseq)
    instSeq.seqPosteriorProbability_=(sp)
  }
  
}

abstract class AbstractViterbi(dynamic: Boolean, segSize: Int, nls: Int) extends DecodingAlgorithm with PotentialScoring with Serializable {
  
  var curNls = nls
  
  val posteriorCrf : Option[DenseCrf]
  
  class SeqLab(val label:Int, val pos:Int, var score:Double, var prev: Option[SeqLab]) {
    
    def this(i: Int, pp: Int) = this(i,pp,(Double.MinValue), None)
    
    def copy(that: SeqLab) = {
      this.score = that.score
      this.prev = that.prev
    }	
    
    def clear() = {
      score = Double.MinValue
    }
  }
  
  class Position(val size: Int, id: Int, pos: Int) {
    val seqArr = Array.tabulate(size){_ => new SeqLab(id,pos)}
    def apply(i:Int) = seqArr(i)
    def clear() = { 
      var i = 0
      while (i < size) {
        seqArr(i).clear()
        i += 1
      }
    }    
    private def insert(i:Int, sc: Double, prev:Option[SeqLab]) = {
      var k = size - 1
      while (k > i) {
        seqArr(k).copy(seqArr(k-1))
        k -= 1
      }
      seqArr(i).score_=(sc)
      seqArr(i).prev_=(prev)
    }    
    private def findInsert(ipos:Int, sc: Double, prev: Option[SeqLab]) = {
      var halt = false
      var i = ipos
      while (!halt && i < size) {
        if (sc > (seqArr(i).score)) {
          insert(i,sc,prev)
          halt = true
        }
        i += 1
      }
      i
    }    
    def addNew(other: Position, sc: Double) = {
      var i = 0
      var ipos = 0
      while ((i < other.size) && ipos < size) {
        val nsc = other(i).score + sc
        ipos = findInsert(ipos,nsc,Some(other(i)))
        i += 1
      }
    }
    def add(sc: Double) = findInsert(0,sc,None)
    def getScores = Array.tabulate(size){i => seqArr(i).score}
  }
  
  protected def viterbiSearch(beamsize:Int, iseq:Seq[AbstractInstance], params: Array[Double]) = {
    if (dynamic) curNls = iseq.length // set current number of labels to size of sequence
    val ri: Array[Array[Double]] = Array.fill(segSize,curNls)(0.0)
    val mi: Array[Array[Array[Double]]] = Array.fill(segSize,curNls,curNls)(0.0)
    val bestAssignment = 
      Array.tabulate(iseq.length){l => Array.tabulate(curNls){yi => new Position((if (l==0) 1 else beamsize),yi,l)}}
    var i = 0
    val slen = iseq.length
    while (i < slen) {
      val inst = iseq(i)
      val instFeatures = inst.getCompVec
      val label = inst.label
      computeScores(ri, mi, instFeatures, false, curNls, params)
      var yi = 0
      while (yi < curNls) {
        bestAssignment(i)(yi).clear()
        yi += 1
      }
      yi = 0
      while (yi < curNls) {
        var d = 0
	  val dTo = (segSize min (i + 1))
        while (d < dTo) {
          val riVal = ri(d)(yi)
          if (i > d) {
            var yp = 0
            while (yp < curNls) {
              val vl = mi(d)(yp)(yi) + riVal
              bestAssignment(i)(yi).addNew(bestAssignment(i - d - 1)(yp), vl)
              yp += 1
            }
          } else {
            bestAssignment(i)(yi).add(riVal)
          }
          d += 1
        }
        yi += 1
      }
      i += 1
    }
    val finalSeq = new Position(beamsize,0,0)
    var yi = 0
    while (yi < curNls) {
      finalSeq.addNew(bestAssignment(slen-1)(yi),0.0)
      yi += 1
    }
    finalSeq
  }
  
  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance], params: Array[Double]) : Double = {
    //if (dynamic) resize(iseq.length)
    val sl = iseq.length
    if (sl > 0) {
      val fsol = viterbiSearch(1, iseq, params)
      var yb: Option[SeqLab] = Some(fsol(0))
      var sid = 0
      var cont = true
      var j = 0
      while (j < sl) {
    	iseq(j).label_=(-1) // make sure to set these to something else
        j += 1
      }
      while (cont) {
        yb match { 
          case Some(ybest) =>   
            val nlab = ybest.label
            val next = ybest.prev
            val prevPos = next match {case Some(p) => p.pos case None => -1}
            var i = ybest.pos
            while (i > prevPos) {
              val inst = iseq(i)
              inst.label_=(nlab)
              inst.segId_=(sid)
              i -= 1
            }
            sid -= 1
            yb = next
          case None => cont = false}
      }
      val sUp = -(iseq(0).segId)
      iseq foreach ((inst) => inst.segId_=(inst.segId + sUp))
    }
    posteriorCrf match {
      case Some(pCrf) =>
        if (iseq.length > 0) {
          // note that pCrf will modify the condition probabilities for each sequence element in place
          // resulting in position marginal posterior distributions available for output
	      math.exp(pCrf.gradOfSeq(iseq))
        } else 0.0
      case None => 0.0}
  }
}

class Viterbi(val dynamic: Boolean, val segSize: Int, crf: CoreModel, computePosterior: Boolean = false) 
extends AbstractViterbi(dynamic, segSize, crf.nls) with PotentialScoring {
  def this(segSize: Int, crf: CoreModel) = this(false, segSize, crf)
  def this(dyn: Boolean, crf: CoreModel) = this(dyn, 1, crf) 
  def this(crf:CoreModel) = this(1,crf)

  val posteriorCrf : Option[DenseCrf] = if (computePosterior) Some(new DenseCRFConfidences(crf) with CondLogLikelihoodLearner[AbstractInstance]) else None

  def getCopyOf = new Viterbi(this.dynamic, this.segSize, this.crf)
  
  CrfInstance.maxSegSize = segSize - 1 // need to set this "globally" for use in decoding, 0-based in CrfInstance
  
  
  protected def computeScores(ri: Array[Array[Double]], mi: Array[Array[Array[Double]]], instFeatures: Array[Array[Feature]], takeExp: Boolean) : Unit = {
    computeScores(ri,mi,instFeatures,takeExp,curNls,crf.params)
  }

  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double = {
    assignBestSequence(iseq, crf.params)
  }  
}

class StatelessViterbi(d: Boolean, ss: Int, n: Int) extends AbstractViterbi(d, ss, n) with Serializable {
  val posteriorCrf = None
  
  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double = {
    throw new RuntimeException("Stateless Viterbi requires passing explicit parameters")
  }
  
  def getCopyOf = new StatelessViterbi(this.d, this.ss, this.n)
}

class NeuralViterbi(dynamic: Boolean, segSize: Int, crf: CoreModel) extends Viterbi(dynamic, segSize, crf) with NeuralStochasticCrfScoring {

  val ngs          = crf.nGates * crf.nls
  val activations  = Array.fill(segSize,ngs)(0.0)
  val numFs        = crf.nfs - (crf.nNfs * crf.nGates) - ngs
  val gateIdx      = crf.nfs - ngs
  val weightedActivationPartials = Array.fill(ngs)(0.0)

  override protected def computeScores(ri: Array[Array[Double]], mi: Array[Array[Array[Double]]], instFeatures: Array[Array[Feature]], takeExp: Boolean) : Unit = {
    computeScores(ri,mi,crf.params,activations(0),weightedActivationPartials,numFs,gateIdx,crf.nls,crf.nGates,crf.nNfs,instFeatures,takeExp)
  }

  override def getCopyOf = new NeuralViterbi(this.dynamic, this.segSize, this.crf)

}
