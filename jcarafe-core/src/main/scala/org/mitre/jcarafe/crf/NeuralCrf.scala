/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import collection.mutable.HashSet
import collection.mutable.HashMap
import org.mitre.jcarafe.util.{Options,OptionHandler}


/**
 * Object used to initialize a Neural CRF.  Extra parameters and book-keeping needs to be set up.
 */
object NeuralCrf {

  type Matrix = Array[Array[Double]]
  type Tensor = Array[Matrix]
  type T = AbstractInstance

  def apply(sgen: SeqGen[_], opts: Options) = {
    val nls = sgen.getNumberOfStates
    val nfs = sgen.getNumberOfFeatures //  number of input features
    val nnfs = sgen.getNumberOfNeuralFeatures
    val nGates : Int = opts.numGates
    val nTotalFeatures = nfs+(nnfs*nGates)+(nGates*nls)
    //println("Number of neural features = " + nnfs)
    //println("nGates = " + nGates)
    if (opts.sgd)
      if (opts.l1)
	new NeuralStochasticCrf(nls,nTotalFeatures,1,opts,nnfs,nGates) with SgdLearnerWithL1[T]
      else
	new NeuralStochasticCrf(nls,nTotalFeatures,1,opts,nnfs,nGates) with SgdLearner[T]
    else if (opts.psa)
      if (opts.l1)
	new NeuralStochasticCrf(nls,nTotalFeatures,1,opts,nnfs,nGates) with PsaLearnerWithL1[T]
      else 
	new NeuralStochasticCrf(nls,nTotalFeatures,1,opts,nnfs,nGates) with PsaLearner[T]
    else {
      if (opts.parallel) {
	val nthds = opts.numThreads match {case Some(v) => v case None => 2}
	new NeuralDenseParallelCrf(nthds,nls,nTotalFeatures,1,opts,nnfs,nGates)
      }
      else
	new NeuralDenseCrf(nls,nTotalFeatures,1,opts,nnfs,nGates) with CondLogLikelihoodLearner[T]
    }
  }
}

abstract class NeuralDenseCrf(lambdas: Array[Double],
		nls: Int, 
				   nfs: Int, 
				   segSize: Int, 
				   opts: Options,
				   nNfs: Int,
				   nGates: Int) extends DenseCrf(lambdas, nls, nfs, segSize, opts.gaussian, nNfs, nGates) with NeuralStochasticCrfScoring {

  def this(nls: Int, nfs: Int, segSize: Int, opts: Options, nNfs:Int = 0, nGates: Int = 2) = 
    this(Array.fill(nfs)(0.0), nls, nfs, segSize, opts, nNfs, nGates)
  val activations : Array[Array[Double]] = Array.fill(segSize,(nGates * nls))(0.0)
  val numInputFeatures = nfs - (nNfs * nGates) - (nGates * nls)
  val gateWeightIdx = nfs - (nGates * nls)
  val weightedActivationPartials : Array[Double] = Array.fill(nGates * nls)(0.0)

  override def initialize() = {
    val gen = new java.util.Random
    var i = 0
    while (i < nfs) {
      lambdas(i) = gen.nextGaussian / 50.0
      i += 1}
  }

/*
  override def regularize() = {
    var i = 0
    val llen = lambdas.length
    while (i < llen) {
      featureExpectations(i) = 0.0 // need to set this to zero again      
      gradient(i) = 0.0
      i += 1
    }
    0.0
  }
*/
  
  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val instFeatures0 = instFeatures(0)
      val nfeas = instFeatures0.length
      while (k < nfeas) {
        val inst = instFeatures0(k)
	if (inst.fid >= 0) {
          if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
	    gradient(inst.fid) -= inst.value
            seqLogLi += lambdas(inst.fid) * inst.value
          }
          if (inst.prv < 0) featureExpectations(inst.fid) += newA(inst.cur) * beta(i)(inst.cur) * inst.value
          else featureExpectations(inst.fid) += curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur) * inst.value
	}
	// expectation for hidden node inputs
	if (inst.nfid >= 0) { // update hidden node inputs
	    var g = 0
	    val offset = label * nGates
	    while (g < nGates) {
	      val outputWeight = weightedActivationPartials(g + offset)
	      if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
		gradient(numInputFeatures + inst.nfid + g*nNfs) -= inst.value * outputWeight
	      }
	      featureExpectations(numInputFeatures + inst.nfid + g*nNfs) += newA(inst.cur) * beta(i)(inst.cur) * outputWeight * inst.value
	      g += 1
	    }
	}
      
        k += 1
      }

      // compute expectations, constraints for hidden node output weights
      for (l <- 0 until nls) {
	for (g <- 0 until nGates) {
	  val pos = g + l*nGates
	  val out = activations(0)(pos)
	  if (l == label) {
	    gradient(gateWeightIdx + pos) -= out
	    seqLogLi += lambdas(gateWeightIdx + pos) * out
	    //println("seq log-li updated for param " + (gateWeightIdx + pos).toString + " by " + (lambdas(gateWeightIdx + pos) * out) + " with out = " + out + " and param = " + lambdas(gateWeightIdx+pos))
	  }
	  featureExpectations(gateWeightIdx + pos) += newA(l) * beta(i)(l) * out
	}
      }
      Array.copy(newA,0,curA,0,curNls)
      assign(curA,(_ / scale(i)))
      i += 1
    }
    seqLogLi
  }

  override def computeScores (inst_features: Array[Array[Feature]], takeExp: Boolean) = {
    computeScores(ri,mi,lambdas,activations(0),weightedActivationPartials,numInputFeatures,gateWeightIdx,nls,nGates,nNfs,inst_features,takeExp)
  }


}

/**
 * Extensions of "linear" CRFs that provide for (some) features to feed into a set of N
 * hidden neurons.
 * How this will work:  Only some of the input features will be fed into the gates.  Each FeatureType will
 * keep track of whether that feature type goes through gates.  After feature extraction has been completed and
 * when the Neural CRF is being started up, extra parameters are added.  Specifically M extra parameters where
 * M = numGates * numNeuralFeatures.  This will simply be added so that the parameter associated with feature i
 * going into gate g is indexed at position nfs * (g+1) + i where 'nfs' is the number of input features and g is zero
 * indexed.
*/
abstract class NeuralStochasticCrf(nls: Int, 
				   nfs: Int, 
				   segSize: Int, 
				   opts: Options,
				   nNfs: Int = 0,
				   nGates: Int = 0) extends StochasticCrf(nls, nfs, segSize, opts, nNfs, nGates) with NeuralStochasticCrfScoring {



  /*
   * The parameters are laid out as follows:
   * 0 to (numInputFeatures - 1)  - weights for input features
   * numInputFeatures to (numInputFeatures + (nNfs * nGates) - 1) - weights for gate input features
   * (numInputFeatures + (nNfs * nGates)) to (numInputFeatures + (nNfs * nGates) + (nGates*nls) - 1) - weights for gate outputs
   *
   * [x_1,....,x_n, g_1_1, g_1_2,..., g_1_m, g_2_1, ..., g_2_m, ...g_G_m, o_1_1,..., o_L_G]
   * where x_i is an input feature
   * g_i_j means the jth "neural" input feature that goes into gate i - (note that the label is encoded in the feature id)
   * o_i_j indicates the parameter for the output of the jth gate associated with the label 'i'
   */

  val activations : Array[Array[Double]] = Array.fill(segSize,(nGates * nls))(0.0)
  val numInputFeatures = nfs - (nNfs * nGates) - (nGates * nls)
  val gateWeightIdx = nfs - (nGates * nls)
  val weightedActivationPartials : Array[Double] = Array.fill(nGates * nls)(0.0)

  override lazy val etas = Array.tabulate(nfs){i => 
    if ((i < numInputFeatures) || (i >= gateWeightIdx)) initialLearningRate 
    else 
      opts.hiddenLearningRate match {case Some(m) => m.toDouble case None => initialLearningRate * 5.0}}

  override def initialize() = {
    val gen = new java.util.Random
    var i = 0
    while (i < nfs) {
      lambdas(i) = gen.nextGaussian / 5.0 // make these quite small
      i += 1}
  }
  
  private def getGradRef(id: Int) = {
    gradient.get(id) match {
      case Some(v) => v 
      case None => 
        val nv = new DoubleCell(0.0,0.0)
        gradient += ((id,nv))
        nv}
  }

  override protected def forwardPass(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    var seqLogLi = 0.0
    var i = 0
    while (i < iseq.length) {
      val instFeatures = iseq(i).getCompVec
      val label = iseq(i).label
      /*
      println("computing scores for inst: " + label)
      println("input features: ")
      instFeatures(0) foreach {i => 
	if ((i.nfid) >= 0) println(" v => " + (gateWeightIdx + i.nfid + i.cur * nGates) + " => " + i.value)
        if ((i.fid) >= 0) println(" v => " + (i.fid) + " => " + i.value)}
	*/

      computeScores(instFeatures,true)
      Array.copy(curA, 0, tmp, 0, curNls)
      matrixMult(mi(0), tmp, newA, 1.0, 0.0, true)
      assign1(newA, ri(0), (_ * _))
      var k = 0
      val instFeatures0 = instFeatures(0)
      val nfeas = instFeatures0.length
      while (k < nfeas) {
        val inst = instFeatures0(k)
	if (inst.fid >= 0) {
          val gref = getGradRef(inst.fid)
          if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
            gref.g_= (gref.g + inst.value)
              seqLogLi += lambdas(inst.fid) * inst.value
          }
          if (inst.prv < 0) gref.e_= ((gref.e + newA(inst.cur) * beta(i)(inst.cur)) * inst.value)
          else gref.e_= ((gref.e + curA(inst.prv) * ri(0)(inst.cur) * mi(0)(inst.prv)(inst.cur) * beta(i)(inst.cur)) * inst.value)
	}
	// expectations for hidden node inputs
	if (inst.nfid >= 0) { // update hidden node inputs
	    var g = 0
	    val offset = label * nGates
	    while (g < nGates) {
	      val outputWeight = weightedActivationPartials(g + offset)
	      val nref = getGradRef(numInputFeatures + inst.nfid + g*nNfs)
	      if ((label == inst.cur) && ((inst.prv < 0) || ((i > 0) && (iseq(i-1).label == inst.prv)))) {
		//println("updating constraint on hidden node input " + inst.nfid + " => " + (inst.value * outputWeight) + " to => " + (nref.g + inst.value * outputWeight))
		//println("done by getting weighted partial output id => " + (g + offset))
		nref.g_=(nref.g + inst.value * outputWeight)
	      }
	      nref.e_=((nref.e + newA(inst.cur) * beta(i)(inst.cur) * outputWeight * inst.value))
	      g += 1
	    }
	}
        k += 1
      }
      // compute expectations, constraints for hidden node output weights
      var l = 0; while (l < nls) {
	var g = 0; while (g < nGates) {
	  val pos = g + l*nGates
	  val oGref = getGradRef(gateWeightIdx + pos)
	  val out = activations(0)(pos)
	  if (l == label) {
	    oGref.g_=(oGref.g + out) // empirical value is just the gate output
	  }
	  oGref.e_=(oGref.e + newA(l) * beta(i)(l) * out) // expectation updated
	  seqLogLi += lambdas(gateWeightIdx + pos) * out
	  g += 1
	}
	l += 1
      }
      Array.copy(newA,0,curA,0,curNls)
      assign(curA,(_ / scale(i)))
      i += 1
    }
    seqLogLi
  }
  
  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = {
    val asize = batchSize min seqAccessor.length
    var gradNormalizer = 0.0
    /*
    println("\n\nCurrent parameters: " )
    lambdas foreach {l => print(" " + l)}
    println("")
*/
    for (i <- curPos until curPos + asize) {
      val j = i % seqAccessor.length
      val iseq = seqAccessor(j)
      if (iseq.length > 0) {
        reset(false,iseq.length)
        gradient.foreach {case (k,v) => v.e_=(0.0)} // reset expectations to zero
        backwardPass(iseq)
        forwardPass(iseq)
	/*
	println("----------------")
	println("constraints for seq with label(0) = " + iseq(0).label)
	gradient.foreach {case (k,v) => print(" " + k + ":" + v.g)}
	println("")
*/
	val pzx = vecSum(curA)
	val zx_p = if (pzx < Double.MaxValue) pzx else Double.MaxValue
        val zx = if ((math.abs(zx_p) < 1.0E-100)) 1.0E-100 else zx_p
/*
	println("expectations: ")
	gradient.foreach {case (k,v) => print(" " + k + ":" + (v.e / pzx))}
	println("")
	println("gradient = " )
	gradient foreach {case (k,cell) =>
	  print(" " + k + ":" + (cell.g - (cell.e / zx)))
	}
	println("")
	println("Outcome distribution: " )
	curA foreach {v => print(" " + (v / pzx))}
	println("\n")
*/
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
    if (gradNormalizer > 100.0) {
    	val nn = 100.0 / gradNormalizer
    	for ((k,cell) <- gradient) cell.g_= ((cell.g * nn)  - lambdas(k) * invSigSqr)
    } else {
    if (invSigSqr > 1E-4) for ((k,cell) <- gradient) cell.g_= (cell.g  - lambdas(k) * invSigSqr) } 
    None
  }

  override def computeScores (inst_features: Array[Array[Feature]], takeExp: Boolean) = {
    computeScores(ri,mi,lambdas,activations(0),weightedActivationPartials,numInputFeatures,
				      gateWeightIdx,nls,nGates,nNfs,inst_features,takeExp)
  }
}

/*
class ParStochasticNeuralCrf(nls: Int, 
				   nfs: Int, 
				   segSize: Int, 
				   opts: Options,
				   nNfs: Int = 0,
				   nGates: Int = 0) 
extends StochasticCrf(nls, nfs, segSize, opts, nNfs, nGates) 
with ParallelStochastic[NeuralStochasticCrf] {

  def getWorker : NeuralStochasticCrf = {
    val newOpts = opts.copy()
    newOpts.maxIters = 1
    newOpts.gaussian = Double.MaxValue
    newOpts.batchSize = 1
    newOpts.CValue = 0.1
    new NeuralStochasticCrf(nls,nfs,segSize,newOpts,nNfs,nGates) with PsaLearner[AbstractInstance]
  }
}
*/

trait NeuralStochasticCrfScoring extends PotentialScoring {


  def computeScores(ri: Matrix, mi: Tensor, lambdas: Array[Double], acts: Array[Double], wActs: Array[Double], numFs: Int, 
		    gateIdx: Int, nls: Int, nGates: Int, nNfs: Int, inst_features: Array[Array[Feature]], takeExp: Boolean) = {

    setMatrix(ri)
    setTensor(mi)
    val dlen = inst_features.length
    var d = 0; while (d < dlen) {
      val klen = inst_features(d).length
      var k = 0; while (k < klen) {
        val inst = inst_features(d)(k)
	if (inst.fid >= 0) {
          if (inst.prv < 0) 
            ri(d)(inst.cur) += lambdas(inst.fid) * inst.value
          else 
            mi(d)(inst.prv)(inst.cur) += lambdas(inst.fid) * inst.value
	}
        k += 1
      }
      k = 0
      d += 1
    }
    computeGateActivations(lambdas,acts,wActs,numFs,gateIdx,nls,nGates,nNfs,inst_features)
    d = 0; while (d < dlen) {
      var k = 0; while (k < nls) {
	var i = 0; while (i < nGates) {
	  val idx = i + k * nGates
	  ri(d)(k) += lambdas(gateIdx + idx) * acts(idx)
	  i += 1}
	k += 1}
      d += 1}
    if (takeExp) {
      val dlen = inst_features.length
      var d = 0; while (d < dlen) {
        var k = 0; while (k < nls) {
          ri(d)(k) = math.exp(ri(d)(k))
          var c = 0; while (c < nls) {
            mi(d)(k)(c) = math.exp(mi(d)(k)(c));
            c += 1
          }
          k += 1
        }
        d += 1
      }
    }
  }
  
  def computeGateActivations(lambdas: Array[Double], acts: Array[Double], wActs: Array[Double], numFs: Int, 
			     gateIdx: Int, nls: Int, nGates: Int, nNfs: Int, inst_features: Array[Array[Feature]]) {
    var i = 0
    while (i < acts.length) { acts(i) = 0.0; i += 1}
    i = 0; while (i < nGates) {
      var d = 0
      val dlen = inst_features.length
      while (d < dlen) {
	val instFeaturesD = inst_features(d)
	var k = 0
	while (k < instFeaturesD.length) {
	  val inst = instFeaturesD(k)
	  val lab = inst.cur // adjust the activation for the gate number and the appropriate label for this feature
	  // update gate 'i' associated with label 'lab'
	  if (inst.nfid >= 0) {
	    acts(i + lab*nGates) += lambdas(numFs + inst.nfid + (i*nNfs)) * inst.value
	  }
	  k += 1
	}
	d += 1
      }
      i += 1
    }
    i = 0; while (i < nGates * nls) {
      // tanh activation
      //acts(i) = (2.0 /(1 + math.exp(-2.0*acts(i)))) - 1 
      //logistic
      acts(i) = (1.0 / (1.0 + math.exp(-1.0 * acts(i))))
      // actual tanh
      //acts(i) = (math.exp(acts(i)) - math.exp(-acts(i))) / (math.exp(acts(i)) + math.exp(-acts(i)))
      i += 1
    }
    
    i = 0; while (i < (nGates * nls)) {
      //tanh update
      //wActs(i) = lambdas(gateIdx + i) * (1.0 - acts(i) * acts(i))
      //logistc update
      wActs(i) = lambdas(gateIdx + i) * (1.0 - acts(i)) * acts(i)
      i += 1
    }
  }


}
