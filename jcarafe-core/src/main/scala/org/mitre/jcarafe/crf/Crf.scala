/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import collection.mutable.HashSet
import collection.mutable.HashMap
import org.mitre.jcarafe.util.Options

trait Trainable {

  val lambdas: Array[Double]
  val numParams : Int

  def getGradient(seqAccessor: AccessSeq) : Option[Double]

  def initialize() : Unit

  def getCoreModel() : CoreModel

  def train(accessSeq: AccessSeq, num: Int = 200,mi: Option[(CoreModel,Int) => Unit] = None) : CoreModel

}

trait DenseTrainable extends Trainable {
  val gradient : Array[Double]
}

trait SparseTrainable extends Trainable {
  class DoubleCell(var g: Double, var e: Double)
  val maxEpochs : Int
  val batchSize : Int
  val C : Double
  val initialLearningRate : Double
  val momentum : Double
  val eta : Double
  val etas : Array[Double]
  var curPos : Int
  val gradient : HashMap[Int,DoubleCell]
  val quiet : Boolean
  val periodSize : Int
  var numGradIssues : Int
  val pAlpha : Double
}

class CoreModel(val params: Array[Double], val nfs: Int, val nls: Int, val nNfs:Int = 0, val nGates: Int = 0) {
  def print() = {
    println("NLS: => " + nls)
    println("NFS: => " + nfs)
    println("Parameters")
    for (i <- 0 until params.length) {
      println("  " + i + " => " + params(i))
    }
  }
}

abstract class AccessSeq {
  def apply(i:Int) : Seq[AbstractInstance]
  def splitAccessor(n:Int) : Seq[AccessSeq]
  def length: Int
  def repermute() : Unit
  def accessSingleInstance(i:Int) : AbstractInstance
  def getSeqs: Seq[InstanceSequence]
}
  //class CachedAccessSeq(fs: Array[String]) extends AccessSeq
class MemoryAccessSeq(iseqs: Seq[InstanceSequence]) extends AccessSeq {
  var seqs = iseqs //permuteSeq(iseqs.filter{s => s.length > 0})
  def getSeqs = iseqs
  def apply(i:Int) = seqs(i).iseq
  def length = seqs.length

  def accessSingleInstance(i:Int) = apply(i)(0)

  private def permuteArray[A](a: Array[A]) = {
    var i = a.length - 1
    while (i >= 0) {
      val j = util.Random.nextInt(i+1) 
      val t = a(i)
      a(i) = a(j)
      a(j) = t
      i -= 1
    }
  }

  private def permuteSeq(s: Seq[InstanceSequence]) : IndexedSeq[InstanceSequence] = {
    val nseqs = s.toArray
    permuteArray(nseqs)
    nseqs.toIndexedSeq
  }
  def repermute() = seqs = permuteSeq(seqs)

  def splitAccessor(n:Int) : Seq[AccessSeq] = {
    val ns = if ((seqs.length % n) == 0) seqs.length / n else (seqs.length / n) + 1 
    for (j <- 0 until n) yield {
      new MemoryAccessSeq(seqs.slice(j*ns,seqs.length min ((j+1)*ns)))
    }
  } 
}

/**
 * Top abstract class representing the core elements and functionality of a sequence-structured
 * Conditional Random Field.  A <code>Crf</code> object is created after feature extraction
 * has occurred.
 * @param nls       Number of labels/states
 * @param nfs       Number of features
 * @param segSize   The size of segments.  Sizes greater than 1 indicate the model is a semi-CRF
 * @param gPrior    The Gaussian prior variance used as a regularizer
 * @param nNfs      Number of neural gate input features (for NeuralCrf)
 * @param nGates    Number of neural gates per label (for NeuralCrf)
 */
abstract class Crf(val lambdas: Array[Double], val nls: Int, val nfs: Int, val segSize: Int, val gPrior: Double, val nNfs: Int, val nGates: Int) 
extends Trainable {

  val numParams = nfs


  def this(nls:Int,nfs:Int,gPrior:Double) = this(Array.fill(nfs)(0.0),nls,nfs,1,gPrior,0,0)
  def this(nls:Int,nfs:Int,segSize:Int) = this(Array.fill(nfs)(0.0),nls,nfs,segSize,10.0,0,0)
  def this(nls:Int, nfs:Int) = this(nls,nfs,1)

  def getCoreModel() = new CoreModel(lambdas,numParams,nls,nNfs,nGates)


  /**
   * When set to <code>true</code>, the Crf will allow the state-space to be dynamically
   * sized - i.e. the number of states is dependent on each sequence
  */
  var adjustible = false
  
  if ((nls < 2) || (nfs < 2)) {
    System.err.println("\n\nEnsure the tagset/label set is properly defined!!")
    throw new RuntimeException("Crf requires 2 or more states/features")
  }
  /**
   * A matrix type as an array of arrays of <code>Double</code>s
   */
  type Matrix = Array[Array[Double]]
  
  /**
   * A tensor object as an array of <code>Matrix</code> objects
   */
  type Tensor = Array[Matrix]

  /**
   * The value of the inverse square of the Gaussian prior 
   */
  val invSigSqr = 1.0 / (gPrior * gPrior)
  
  /**
   * For each segment size (general case) the ri matrix holds state scores for each label
   */
  var ri: Matrix = Array.fill(segSize,nls)(0.0)
  /**
   * For each segment size, the mi matrix holds transition scores for adjacent labels
  */
  var mi: Tensor = Array.fill(segSize,nls,nls)(0.0)
  /**
   * Current alpha values used for Forward-Backward computation
   */
  var curA: Array[Double] = Array.fill(nls)(0.0)
  /**
   * Alpha values at the next position used for Forward-Backward computation
   */
  var newA: Array[Double] = Array.fill(nls)(0.0)
  
  var tmp: Array[Double] = Array.fill(nls)(0.0)
  
  /**
   * An array of scaling coefficients to avoid underflow without having to do computations
   * in log space.  See Manning and Schutze Chapter 9 for details (there in the context of HMMs)
   */
  var scale: Array[Double] = Array.fill(1)(0.0)
  
  /**
   * Beta values. Need values for each segment length for each label (in general, Semi-CRF case)  
   */
  var beta: Matrix = Array.fill(1,nls)(0.0)
  
  var curNls = nls
  
  /**
   * Alpha values. Need values for each segment length for each label (in general, Semi-CRF case)  
   */
  var alpha: Matrix = Array.fill(1,nls)(0.0)

  def initialize() = {}

  protected def reset(all: Boolean, slen: Int) : Unit = {
    val aNls = if (adjustible) slen else nls
    if (adjustible) {
      ri = Array.fill(segSize,slen)(0.0)
      mi = Array.fill(segSize,slen,slen)(0.0)
      curA = Array.fill(slen)(0.0)
      newA = Array.fill(slen)(0.0)
      tmp = Array.fill(slen)(0.0)
      var i = 0
      while (i < beta.length) {
	beta(i) = Array.fill(aNls)(0.0)
	i += 1
      }
    }
    if (adjustible) curNls = slen
    if (all && (alpha.length < slen)) alpha = Array.fill(2*slen,aNls)(0.0)
    else for (i <- 0 until aNls) curA(i) = 1.0
    if (beta.length < slen) {
      beta = Array.fill(2*slen,aNls)(0.0)
      scale = Array.fill(2*slen)(0.0)      
    }
  }
  
  protected def computeScores(inst_features:Array[Array[Feature]], takeExp: Boolean) = 
    Crf.computeScores(ri, mi, inst_features, takeExp, curNls, lambdas)
  
  protected def vecSum(vec:Array[Double]) = { 
    var s = 0.0; var i = 0; 
    while(i < vec.length) { s += vec(i); i += 1 }
    s
  }
  
  protected def assign(v1: Array[Double], f:(Double => Double)) = {
    var i = 0;
    val vlen = v1.length
    while (i < vlen) { v1(i) = f(v1(i)); i += 1}
  }
  
  protected def assign1(v1:Array[Double],v2:Array[Double],f:((Double,Double) => Double)) = {
    var i = 0;
    val vlen = v1.length
    while (i < vlen) {v1(i) = f(v1(i),v2(i)); i += 1}
  }
  
  protected def backwardPass(iseq:Seq[AbstractInstance]) = {
    val len = iseq.length
    scale(len-1) = curNls
    assign(beta(len-1),((_:Double) => 1 / scale(len-1)))
    var i = len - 1
    var icheck = false
    while (i > 0) {
      computeScores(iseq(i).getCompVec,true)
      Array.copy(beta(i), 0, tmp, 0, curNls)
      assign1(tmp, ri(0), (_ * _))
      Crf.matrixMult(mi(0), tmp, beta(i-1), 1.0, 0.0, false)
      scale(i-1) = vecSum(beta(i-1))
      assign(beta(i-1),(_ / scale(i-1)))
      i -= 1
    }
  }
    
  protected def forwardPass(iseq:Seq[AbstractInstance]) : Double
  def getGradient(seqAccessor: AccessSeq) : Option[Double]
  
  def train(seqAccessor: AccessSeq) : CoreModel = train(seqAccessor, 300,None)
  def train(seqAccessor: AccessSeq, maxIters: Int, mi: Option[(CoreModel,Int) => Unit]) : CoreModel
  
}

/**
 * A CRF that uses a dense (rather than sparse) internal representation
 */
abstract class DenseCrf(lambdas: Array[Double], nls: Int, nfs: Int, segSize: Int, gPrior: Double, nNfs: Int, nGates: Int) 
extends Crf(lambdas, nls, nfs, segSize, gPrior, nNfs, nGates) {
    
  def this(core: CoreModel) = this(core.params,core.nls,core.nfs,1,1E200,core.nNfs,core.nGates)
  def this(nls:Int,nfs:Int,segSize:Int,gPrior:Double,nNfs: Int = 0, nGates:Int=0) = this(Array.fill(nfs)(0.0),nls,nfs,segSize,gPrior,nNfs, nGates)
  
  val gradient: Array[Double] = Array.fill(nfs)(0.0)
  val featureExpectations: Array[Double] = Array.fill(nfs)(0.0)
  
  def regularize() = {
    var i = 0
    var llMod = 0.0
    val llen = lambdas.length
    while (i < llen) {
      val li = lambdas(i)
      gradient(i) = li * invSigSqr
      featureExpectations(i) = 0.0 // need to set this to zero again
      llMod += (li * li * invSigSqr) / 2.0
      i += 1
    }
    llMod
  }
  
  protected def forwardPass(iseq:Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val instFeatures0 = instFeatures(0)
      val nfeas = instFeatures0.length
      while (k < nfeas) {
        val inst = instFeatures0(k)
        if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
          gradient(inst.fid) -= inst.value
          seqLogLi += lambdas(inst.fid) * inst.value
        }
        if (inst.prv < 0) featureExpectations(inst.fid) += newA(inst.cur) * beta(i)(inst.cur) * inst.value
        else featureExpectations(inst.fid) += curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value
        k += 1
      }
      Array.copy(newA,0,curA,0,curNls)
      assign(curA,(_ / scale(i)))
      i += 1
    }
    seqLogLi
  }
  
  def gradOfSeq(iseq:Seq[AbstractInstance]) : Double = {
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
      //println("constraint: " + gradient(i) + " and expectation: " + (featureExpectations(i) / zx))
      gradient(i) += featureExpectations(i) / zx; i += 1
    }
    sll
  }
  
  def getGradient(seqAccessor: AccessSeq) : Option[Double] = getGradient(true,seqAccessor)

  def getGradient(l2: Boolean, seqAccessor: AccessSeq) : Option[Double] = {
    var logLi = if (l2) regularize() else 0.0
    for (j <- 0 until seqAccessor.length) {
      val seq = seqAccessor(j)
      if (seq.length > 0) logLi -= gradOfSeq(seq)
    }
    Some(logLi)           
  }
}

/**
 * A Crf that uses a sparse internal representation suitable for Stochastic Gradient Descent learning
 * methods.
*/
abstract class StochasticCrf(lambdas: Array[Double],
                             nls: Int, 
			     nfs: Int, 
			     segSize: Int, 
			     val opts: Options, 
			     nNfs: Int,
			     nGates: Int) 
extends Crf(lambdas, nls, nfs, segSize, opts.gaussian, nNfs, nGates) 
with SparseTrainable
{
  def this(nls: Int, nfs: Int, segSize: Int, opts: Options, nNfs: Int = 0, nGates: Int = 0) = this(Array.fill(nfs)(0.0),nls,nfs,segSize,opts,nNfs,nGates)

  val quiet = false

  val periodSize          = opts.psaPeriodSize
  val maxEpochs           = opts.maxIters
  val batchSize           = opts.batchSize
  val C                   = opts.CValue
  val initialLearningRate = opts.learningRate
  val momentum            = opts.momentum
  val pAlpha              = opts.pAlpha

  var numGradIssues = 0
  lazy val etas     = Array.fill(nfs)(initialLearningRate) // only used for PSA-based learning, hence its laziness
  val eta           = initialLearningRate
  val gradient: HashMap[Int,DoubleCell] = new HashMap[Int,DoubleCell]()
  var curPos: Int   = 0

  // used for parallel training methods
  def setNewParams(ls: Array[Double]) = Array.copy(ls, 0, lambdas, 0, nfs)

  // used for parallel training methods
  def setNewEtas(es: Array[Double]) = Array.copy(es, 0, etas, 0, nfs)
  
  protected def forwardPass(iseq:Seq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      Crf.matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val nfeas = instFeatures(0).length
      val instFeatures0 = instFeatures(0)
      while (k < nfeas) {
        val inst = instFeatures0(k)
        val gref = gradient.get(inst.fid) match {
          case Some(v) => v 
          case None => 
            val nv = new DoubleCell(0.0,0.0)
            gradient.update(inst.fid,nv)
            nv}
        if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
          gref.g_= (gref.g + inst.value)
          seqLogLi += lambdas(inst.fid) * inst.value
        }
        if (inst.prv < 0) gref.e_= ((gref.e + newA(inst.cur) * beta(i)(inst.cur)) * inst.value)
        else gref.e_= ((gref.e + curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur)) * inst.value)
        k += 1
      }
      Array.copy(newA,0,curA,0,curNls)
      assign(curA,(_ / scale(i)))
      i += 1
    }
    seqLogLi
  }

  protected def reset(l: Int) : Unit = reset(false, l)
  
  def getGradient(seqAccessor: AccessSeq) = {
    val asize = batchSize min seqAccessor.length
    var gradNormalizer = 0.0
    //var normFactor = 0
    for (i <- curPos until curPos + asize) {
      val j = i % seqAccessor.length
      val iseq = seqAccessor(j)
      val sl = iseq.length
      //normFactor += sl
      if (sl > 0) {
        reset(iseq.length)
        gradient.foreach {case (k,v) => v.e_=(0.0)} // reset expectations to zero
        backwardPass(iseq)
        forwardPass(iseq)
        val pzx = vecSum(curA)
        val zx = if (pzx < Double.MaxValue) pzx else Double.MaxValue
        for ((k,cell) <- gradient) {
          cell.g_= (cell.g - (cell.e / zx))
          val cabs = math.abs(cell.g)
          if (cabs > gradNormalizer) { gradNormalizer = cabs }
        }
      }
    }
    curPos += asize
    // normalization here will prevent gradient components from having a value greater than 100.0
    // Such values in the gradient are problematic in subsequent numerical calculations
    if (gradNormalizer > 50.0) {
      numGradIssues += 1
    	val nn = 50.0 / gradNormalizer
    	for ((k,cell) <- gradient) cell.g_= ((cell.g * nn)  - lambdas(k) * invSigSqr)
    } else {
    for ((k,cell) <- gradient) {
      cell.g_= (cell.g - lambdas(k) * invSigSqr)
    }
    }
    
    None
  }
}  

/**
 * A set of static methods used by Crf objects as well as by decoders (e.g. Viterbi) 
 */
object Crf {
  type Matrix = Array[Array[Double]]
  type Tensor = Array[Matrix]

  def apply(core: CoreModel) = {
    val crf = new DenseCrf(core) with CondLogLikelihoodLearner
    
  }
  
  final def matrixMult(mat:Matrix, vec: Array[Double], rvec: Array[Double], alpha:Double, beta:Double, trans: Boolean) = {
    val vLen = vec.length
    val rvLen = rvec.length
    if (trans) {
      var i = 0; 
      while(i < rvLen) {
        var r = 0.0;
        var j = 0; while(j < vLen) {
          r += vec(j) * mat(j)(i)
          j += 1
        }
        rvec(i) = rvec(i) * beta + r * alpha
        i += 1
      }
    } else {
      var i = 0; 
      while(i < rvLen) {
        var r = 0.0;
        var j = 0; 
        while(j < vLen) {
          r += vec(j) * mat(i)(j)
          j += 1
        }
        rvec(i) = rvec(i) * beta + r * alpha
        i += 1
      }
    }
  }
 
  final def setMatrix (m: Matrix) = {
    var i = 0
    var j = 0
    val mlen = m.length
    while (i < mlen) {
      j = 0
      val milen = m(i).length
      while (j < milen) {
        m(i)(j) = 0.0
        j += 1
      }
      i += 1
    }
  }

  final def setTensor (t: Tensor) = {
    var i = 0
    val tlen = t.length
    while (i < tlen) {
      setMatrix(t(i))
      i += 1
    }
  }
  
  final def computeScores (ri: Matrix, mi: Tensor, inst_features: Array[Array[Feature]], takeExp: Boolean, nls: Int, lambdas: Array[Double]) = {
    setMatrix(ri)
    setTensor(mi)
    val dlen = inst_features.length
    var d = 0; while (d < dlen) {
      val instD = inst_features(d)
      val klen = instD.length
      var k = 0; while (k < klen) {
        val inst = instD(k)
	val rid = ri(d)
	val mid = mi(d)
	if (inst.fid >= 0) {
          if (inst.prv < 0) {
            rid(inst.cur) += lambdas(inst.fid) * inst.value
          } else {
            mid(inst.prv)(inst.cur) += lambdas(inst.fid) * inst.value
	  }
	}
        k += 1
      }
      k = 0
      d += 1
    }
    if (takeExp) {
      val dlen = inst_features.length
      var d = 0; while (d < dlen) {
	val rd = ri(d)
	val md = mi(d)
        var k = 0; while (k < nls) {
	  val mdk = md(k)
          rd(k) = math.exp(rd(k))
          var c = 0; while (c < nls) {
            mdk(c) = math.exp(mdk(c));
            c += 1
          }
          k += 1
        }
        d += 1
      }
    }
  }	
}