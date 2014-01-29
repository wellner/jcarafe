/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import org.mitre.jcarafe.optimize.{ConvexOptimizer, FunctionEvaluation, LbfgsOptimizer, Params}
import org.mitre.jcarafe.util.FastLoops._

trait CrfLearner {

  def print_zero_wt_feature_cnt(weights: Array[Double], num_features: Int) = {
    var num_zero_wt_features = 0
    var num_non_zero_wt_features = 0
    var index = 0
    while (index < num_features) {
      if (weights(index) == 0.0) { num_zero_wt_features += 1 }
      else { num_non_zero_wt_features += 1 }
      index += 1
    }
    println("\n ..There are " + num_features + " total features: " + num_zero_wt_features + " have a zero weight and " + num_non_zero_wt_features + " have a non-zero weight.")
  }
}

trait CondLogLikelihoodLearner[T] extends DenseTrainable[T] with CrfLearner {

  def train(accessSeq: AccessSeq[T], max_iters: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None): CoreModel = {
    var iter = 0
    println("Conditional Maximum Likelihood Training - Batch optimization using L-BFGS method")
    println("There are " + accessSeq.length + " training sequences")
    println("\t Number of features/parameters:  " + numParams)
    println("\t Maximum iterations = " + max_iters)    
    initialize()
    val t_s = System.nanoTime
    val fn = new FunctionEvaluation {
      def evaluate(x: Array[Double], g: Array[Double], n: Int, st: Double) = {
        
        val Some(f) = getGradient(accessSeq)
        f
      } 
    }
    val p = new Params
    p.maxIters_=(max_iters)
    p.verbose_=(true)
    val optimizer = new LbfgsOptimizer(lambdas,gradient,fn,p)
    val result = optimizer.optimize()
    println("\n...L-BFGS optimization complete; status: " + result.status)
    println("...Negative log-likelihood: \t" + result.objective)
    println("...Gradient norm: \t\t" + result.gnorm)
    println("...Number of iterations: \t" + result.numIterations)
    println("...Training completed in \t" + ((System.nanoTime - t_s) / 1000000000.0) + " seconds")
    print_zero_wt_feature_cnt(lambdas, numParams)
    getCoreModel()
  }
}

/*
SGD training routine
Use exponential decay: eta_t = eta_0*p_alpha^(t/N)
  where t is the iteration counter (called 'k' in the paper where this came from)
  and N is the number of training examples.
  The paper uses 'alpha' instead of 'p_alpha'.
  Version in the paper has (- t/N) instead of (t/N), but for alpha < 1, this will
    increase eta_t as t increases (one wants eta_t to decrease as t increases).
  This may decay too slowly when batchSize > 1.
  So to handle batchSize > 1, actually use the expression:
    eta_t = eta_0*p_alpha^(batchSize*t/N)
From "Stochastic Gradient Descent Training for L1-regularized Log-linear Models with Cumulative Penalty"
By Y. Tsuruoka, J. Tsujji and S. Ananiadou (ACL 2009).
   Alex Yeh, 2009Nov
*/
trait SgdLearner[T] extends SparseTrainable[T] with CrfLearner {

  val p_alpha = pAlpha //Some possible values include: 0.9, 0.85, 0.8
  val eta_0 = eta //Some possible values include: 1.0, 0.5, 0.2, 0.1
  

  def train(accessSeq: AccessSeq[T], x: Int,modelIterFn: Option[(CoreModel,Int) => Unit] = None): CoreModel = {
    var eta_t = eta_0 //Dummy value (indicates type). The value will be replaced.
    val N: Double = accessSeq.length //Force N to be floating point so that 't/N' is done in floating point and not as truncated or rounded-off integer division
    var converged = false
    var t = 0
    var epochLL = 0.0
    val max_iters = accessSeq.length * maxEpochs / batchSize
    initialize()
    accessSeq.repermute()
    val previousDeltas: Option[Array[Double]] = if (momentum > 0.0) Some(Array.fill(numParams)(0.0)) else None

    if (!quiet) {
      println("Stochastic Gradient Descent Training over " + accessSeq.length + " sequences")
      println("\t maxEpochs= " + maxEpochs + "; batchSize= " + batchSize + "; max_iters= " + max_iters)
      println("\t N= " + N + "; eta_0= " + eta_0 + "; p_alpha= " + p_alpha)
      if (momentum > 0.0) println("Momentum coefficient = " + momentum)
    }
    val t_s = System.nanoTime
    while ((!converged) && (t < max_iters)) {
      //eta_t = eta_0 * scala.math.pow(p_alpha, (batchSize * t / N))
      curPos = (t * batchSize) % accessSeq.length
      gradient.clear() // clear map on each iteration
      val sll = getGradient(accessSeq)
      epochLL += sll.getOrElse(0.0)
      previousDeltas match {
        case Some(prevs) =>
          for ((k, v) <- gradient) {
            prevs(k) = eta_t * v.g + momentum * prevs(k)
            lambdas(k) += prevs(k)
          }
        case None =>
          for ((k, v) <- gradient) {
            lambdas(k) += eta_t * v.g
          }
      }

      t += 1
      if ((t % accessSeq.length) == 0 && (t > 0)) {
        println("Epoch " + (t / accessSeq.length) + " complete (of " + maxEpochs + ")") //This statement may be incorrect for batchSize > 1?
        if (epochLL > 0.001) println("Log-likelihood for Epoch: " + epochLL)
        epochLL = 0.0
        accessSeq.repermute()
      }
    }
    if (!quiet) println("\n...Training completed in " + ((System.nanoTime - t_s) / 1000000000.0) + " seconds")
    print_zero_wt_feature_cnt(lambdas, numParams)
    getCoreModel()
    //new CoreModel(lambdas,numParams,nls,nNumParams,nGates)
  }
}

/*
SGD + L1 regularization training routine
Use exponential decay: eta_t = eta_0*p_alpha^(t/N)
  where t is the iteration counter (called 'k' in the paper where this came from)
  and N is the number of training examples.
  The paper uses 'alpha' instead of 'p_alpha'.
  Version in the paper has (- t/N) instead of (t/N), but for alpha < 1, this will
    increase eta_t as t increases (one wants eta_t to decrease as t increases).
  This may decay too slowly when batchSize > 1.
  So to handle batchSize > 1, actually use the expression:
    eta_t = eta_0*p_alpha^(batchSize*t/N)
From "Stochastic Gradient Descent Training for L1-regularized Log-linear Models with Cumulative Penalty"
By Y. Tsuruoka, J. Tsujji and S. Ananiadou (ACL 2009).
   Alex Yeh, 2009Dec
*/
trait SgdLearnerWithL1[T] extends SparseTrainable[T] with CrfLearner {

  val p_alpha = pAlpha //Some possible values include: 0.9, 0.85, 0.8
  val eta_0 = eta //Some possible values include: 1.0, 0.5, 0.2, 0.1
  val q = Array.fill(numParams)(0.0) //q(k): total amount of regularization used so far on lambda(k) to try to minimize its magnitude
  //The total is an arithmetic sum, with positive and negative amounts canceling each other.

  def train(accessSeq: AccessSeq[T], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None): CoreModel = {
    var eta_t = eta_0 //Dummy value (indicates type). The value will be replaced.
    val N: Double = accessSeq.length //Force N to be floating point so that 't/N' is done in floating point and not as truncated or rounded-off integer division
    var u = 0.0 //Maximum cumulative regularization magnitude so far
    var epochLL = 0.0
    val previousDeltas: Option[Array[Double]] = if (momentum > 0.0) Some(Array.fill(numParams)(0.0)) else None
    var converged = false
    var t = 0
    val max_iters = accessSeq.length * maxEpochs / batchSize
    //Knowing the most positive and negative gradient values encountered may help with setting C, the regularization rate, in the future.
    var most_pos_grad_val_so_far = 0.0
    var most_neg_grad_val_so_far = 0.0

    if (!quiet) {
      println("Stochastic Gradient Descent Training with L1 regularization over " + accessSeq.length + " sequences")
      println("\t maxEpochs= " + maxEpochs + "; batchSize= " + batchSize + "; max_iters= " + max_iters)
      println("\t N= " + N + "; eta_0= " + eta_0 + "; p_alpha= " + p_alpha + "; C= " + C)
    }
    val t_s = System.nanoTime
    while ((!converged) && (t < max_iters)) {
      //eta_t = eta_0 * scala.math.pow(p_alpha, (batchSize * t / N))
      u += (eta_t * batchSize * C / N) //Have u accumulate "eta_t*C" each time all the training sequences have been checked
      curPos = (t * batchSize) % accessSeq.length
      gradient.clear() // clear map on each iteration
      val sll = getGradient(accessSeq)
      epochLL += sll.getOrElse(0.0)
      for ((k, v) <- gradient) {
        previousDeltas match {
          case Some(prevs) =>
            prevs(k) = eta_t * v.g + momentum * prevs(k)
            lambdas(k) += prevs(k)
          case None => lambdas(k) += eta_t * v.g
        }
        if (v.g > 0.0) { most_pos_grad_val_so_far = scala.math.max(most_pos_grad_val_so_far, v.g) }
        else { most_neg_grad_val_so_far = scala.math.min(most_neg_grad_val_so_far, v.g) }
        //ApplyPenalty to lambdas(k)
        val z = lambdas(k)
        if (z > 0) lambdas(k) = scala.math.max(0.0, z - (u + q(k)))
        else if (z < 0) lambdas(k) = scala.math.min(0.0, z + (u - q(k)))
        q(k) += (lambdas(k) - z)
      }
      t += 1
      if ((t % accessSeq.length) == 0 && (t > 0)) println("Epoch " + (t / accessSeq.length) + " complete (of " + maxEpochs + "). Most + gradient so far= " + most_pos_grad_val_so_far + "; most - gradient so far= " + most_neg_grad_val_so_far) //This statement may be incorrect for batchSize > 1?
      if (epochLL > 0.001) println("Log-likelihood for Epoch: " + epochLL)
      epochLL = 0.0      
      
    }
    if (!quiet) {
      println("\n...Training completed in " + ((System.nanoTime - t_s) / 1000000000.0) + " seconds")
      println("Most + gradient found= " + most_pos_grad_val_so_far + "; most - gradient found= " + most_neg_grad_val_so_far)
    }
    print_zero_wt_feature_cnt(lambdas, numParams)
    getCoreModel()
    //new CoreModel(lambdas,numParams,nls,nNfs,nGates)
  }
}

/*
2009Dec.: Adapted for PSA from "Stochastic Gradient Descent Training for L1-regularized Log-linear Models with Cumulative Penalty"
By Y. Tsuruoka, J. Tsujji and S. Ananiadou (ACL 2009).
*/
trait PsaLearnerWithL1[T] extends SparseTrainable[T] with CrfLearner {
  val p_alpha = 0.9999
  val p_beta = 0.99
  val n = periodSize
  //val k = 1000.0 
  val k = 0.95
  val params_n = Array.fill(numParams)(0.0)
  val params_t = Array.fill(numParams)(0.0)
  val big_m = ((p_alpha + p_beta) * k) / (p_alpha - p_beta)
  val small_m = (2 * (1 - p_alpha) * k) / (p_alpha - p_beta)
  val uVec = Array.fill(numParams)(0.0) //Maximum cumulative regularization magnitude so far for each feature
  val uVec_last_update_t = Array.fill(numParams)(-1) //For the kth value, the iteration during which the last update to uVec(k) was performed. Initially, uVec is deemed to be last updated just before the first iteration (iteration i=0).
  val q = Array.fill(numParams)(0.0) //q(k): total amount of regularization used so far on lambda(k) to try to minimize its magnitude

  def train(accessSeq: AccessSeq[T], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None): CoreModel = {
    val big_n_for_l1: Double = accessSeq.length //Force 'big_n_for_l1' ('N' in the paper) to be floating point so that 't/big_n_for_l1' is done in floating point and not as truncated or rounded-off integer division
    var converged = false
    var t = 0
    val max_iters = accessSeq.length * maxEpochs / batchSize
    val num_zero_val = big_m / (big_m + k + small_m)
    var epochLL = 0.0
    //Knowing the most positive and negative gradient values encountered may help with setting C, the regularization rate, in the future.
    var most_pos_grad_val_so_far = 0.0
    var most_neg_grad_val_so_far = 0.0
    //var most_pos_lambda_so_far = 0.0
    //var most_neg_lambda_so_far = 0.0
    val previousDeltas: Option[Array[Double]] = if (momentum > 0.0) Some(Array.fill(numParams)(0.0)) else None
    if (!quiet) {
      println("\nStochastic Gradient Descent Training (with PSA and L1 regularization) over " + accessSeq.length + " sequences")
      println("\t maxEpochs= " + maxEpochs + "; batchSize= " + batchSize + "; max_iters= " + max_iters)
      println("\t The eta's are initialized to " + etas(0))
      if (momentum > 0.0) println("Momentum coefficient = " + momentum)
      println("\t p_alpha= " + p_alpha + "; p_beta= " + p_beta + "; n= " + n + "; k= " + k + "; big_m= " + big_m + "; small_m= " + small_m + "; C= " + C)
    }
    val t_s = System.nanoTime
    while ((!converged) && (t < max_iters)) {
      curPos = (t * batchSize) % accessSeq.length
      gradient.clear // clear map on each iteration
      val sll = getGradient(accessSeq)
      epochLL += sll.getOrElse(0.0)
      for ((k, v) <- gradient) {
        previousDeltas match {
          case Some(prevs) =>
            prevs(k) = etas(k) * v.g + momentum * prevs(k)
            lambdas(k) += prevs(k)
          case None =>
            lambdas(k) += etas(k) * v.g
        }
        /*
	if (lambdas(k) > most_pos_lambda_so_far) {
	  most_pos_lambda_so_far = lambdas(k)
	  println("most pos lambda = " + lambdas(k)) }
	else if (lambdas(k) < most_neg_lambda_so_far) {
	  most_neg_lambda_so_far = lambdas(k)
	  println("most neg lambda = " + lambdas(k))
	}
	*/
        if (v.g > 0.0) { most_pos_grad_val_so_far = scala.math.max(most_pos_grad_val_so_far, v.g) }
        else { most_neg_grad_val_so_far = scala.math.min(most_neg_grad_val_so_far, v.g) }
        //Before apply penalty, update uVec(k) & reset when it was last updated
        val num_iters_since_last_update = t - uVec_last_update_t(k)
        uVec(k) += (num_iters_since_last_update * etas(k) * batchSize * C / big_n_for_l1)
        uVec_last_update_t(k) = t

        //ApplyPenalty to lambdas(k): COPIED FROM SGD L1 and modified to use vector of U values
        val z = lambdas(k) //z= lambda(k) before penalty
        if (z > 0) lambdas(k) = scala.math.max(0.0, z - (uVec(k) + q(k)))
        else if (z < 0) lambdas(k) = scala.math.min(0.0, z + (uVec(k) - q(k)))
        q(k) += (lambdas(k) - z)
      }

      if (((t + 1) % (n * 2)) == 0) {
        var i = 0
        while (i < numParams) {
          val num_iters_since_last_update = t - uVec_last_update_t(i)
          uVec(i) += (num_iters_since_last_update * etas(i) * batchSize * C / big_n_for_l1)
          uVec_last_update_t(i) = t

          val denom = params_n(i) - params_t(i)
          val numer = lambdas(i) - params_n(i)
          val bi = {
            if (denom == 0.0) { if (numer < 0) p_beta else p_alpha } // avoid some calculations by just returning alpha or beta depending on sign
            else if (numer == 0.0) num_zero_val
            else {
              val gamma_i = numer / denom
              val ai = if (gamma_i < 0.0) gamma_i max -k else gamma_i min k
              (big_m + ai) / (big_m + k + small_m)
            }
          }
          etas(i) *= bi
          params_t(i) = lambdas(i)
          i += 1
        }
        print(".")
      } else if (((t + 1) % n) == 0) Array.copy(lambdas, 0, params_n, 0, numParams)
      t += 1
      if ((t % accessSeq.length) == 0 && (t > 0)) println("Epoch " + (t / accessSeq.length) + " complete (of " + maxEpochs + ")")
      if (epochLL > 0.001) println("Log-likelihood for Epoch: " + epochLL)
        epochLL = 0.0
    }
    if (!quiet) {
      println("\n...Training completed in " + ((System.nanoTime - t_s) / 1000000000.0) + " seconds")
      println("Most + gradient found= " + most_pos_grad_val_so_far + "; most - gradient found= " + most_neg_grad_val_so_far)
      print_zero_wt_feature_cnt(lambdas, numParams)
      if (numGradIssues > 0) 
        println("\n ... WARNING: Number of gradient overflow instances: " + numGradIssues)
    }
    getCoreModel()
    //new CoreModel(lambdas,numParams,nls,nNfs,nGates)
  }
}

trait PsaLearner[T] extends SparseTrainable[T] with CrfLearner {

  val p_alpha = 0.9999
  val p_beta = 0.99
  val n = periodSize
  //val k = 1000.0
  val k = 0.95
  val params_n = Array.fill(numParams)(0.0)
  val params_t = Array.fill(numParams)(0.0)
  val big_m = ((p_alpha + p_beta) * k) / (p_alpha - p_beta)
  val small_m = (2 * (1 - p_alpha) * k) / (p_alpha - p_beta)

  def train(accessSeq: AccessSeq[T], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None): CoreModel = {
    var converged = false
    var t = 0
    val max_iters = accessSeq.length * maxEpochs / batchSize
    val num_zero_val = big_m / (big_m + k + small_m)
    val previousDeltas: Option[Array[Double]] = if (momentum > 0.0) Some(Array.fill(numParams)(0.0)) else None
    var epochLL = 0.0
    initialize()
    accessSeq.repermute()
    if (!quiet) {
      println("\nStochastic Gradient Descent Training (with PSA) over " + accessSeq.length + " instances")
      println("\t maxEpochs= " + maxEpochs + "; batchSize= " + batchSize + "; max_iters= " + max_iters)
      println("\t The eta's are initialized to " + etas(0))
      if (momentum > 0.0) println("Momentum coefficient = " + momentum)
      println("\t p_alpha= " + p_alpha + "; p_beta= " + p_beta + "; n= " + n + "; k= " + k + "; big_m= " + big_m + "; small_m= " + small_m)
    }

    val t_s = System.nanoTime
    var gnorm = 0.0
    while ((!converged) && (t < max_iters)) {
      curPos = (t * batchSize) % accessSeq.length
      gradient.clear // clear map on each iteration
      val sll = getGradient(accessSeq)
      epochLL += sll.getOrElse(0.0)
      previousDeltas match {
        case Some(prevs) =>
          for ((k, v) <- gradient) {
            prevs(k) = etas(k) * v.g + momentum * prevs(k)
            lambdas(k) += prevs(k)
          }
        case None =>
          for ((k, v) <- gradient) {
            lambdas(k) += etas(k) * v.g
          }
      }
      if (((t + 1) % (n * 2)) == 0) {
        var i = 0
        while (i < numParams) {
          val denom = params_n(i) - params_t(i)
          val numer = lambdas(i) - params_n(i)
          val bi = {
            if (denom == 0.0) { if (numer < 0) p_beta else p_alpha } // avoid some calculations by just returning alpha or beta depending on sign
            else if (numer == 0.0) num_zero_val
            else {
              val gamma_i = numer / denom
              val ai = if (gamma_i < 0.0) gamma_i max -k else gamma_i min k
              (big_m + ai) / (big_m + k + small_m)
            }
          }
          etas(i) *= bi
          params_t(i) = lambdas(i)
          i += 1
        }
        if (!quiet) print(".")
      } else if (((t + 1) % n) == 0) Array.copy(lambdas, 0, params_n, 0, numParams)
      t += 1
      
      if (!quiet && ((t % accessSeq.length) == 0 && (t > 0))) {
        accessSeq.repermute()
        modelIterFn match {
          case Some(mf) => mf(getCoreModel(), (t / accessSeq.length))
          case None =>
        }
        println("Epoch " + (t / accessSeq.length) + " complete (of " + maxEpochs + ")") //This statement may be incorrect for batchSize > 1?
        if (epochLL > 0.001) println("Log-likelihood for Epoch: " + epochLL)
        epochLL = 0.0
      }
    }
    if (!quiet) {
      println("\n...Training completed in " + ((System.nanoTime - t_s) / 1000000000.0) + " seconds")
      print_zero_wt_feature_cnt(lambdas, numParams)
      if (numGradIssues > 0) 
        println("\n ... WARNING: Number of gradient overflow instances: " + numGradIssues)
    }
    getCoreModel()
  }
}

