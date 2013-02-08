/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.Options
import scala.concurrent.{Await, Future, ExecutionContext, Promise}

trait DenseWorker extends DenseCrf {
  override def train(a:AccessSeq[AbstractInstance]) = throw new RuntimeException("Class doesn't support training")
  override def train(a:AccessSeq[AbstractInstance], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None) = throw new RuntimeException("Class doesn't support training")
}

class DenseCrfWorker(lambdas: Array[Double], nls: Int,nfs: Int,segSize: Int,gPrior: Double) 
extends DenseCrf(lambdas,nls,nfs,segSize,gPrior,0,0) with DenseWorker


class NeuralDenseCrfWorker(lambdas: Array[Double], nls: Int,nfs: Int,segSize: Int,gPrior: Double, nNfs: Int, nGates: Int, opts: Options) 
extends NeuralDenseCrf(lambdas, nls, nfs, segSize, opts, nNfs,nGates) with DenseWorker


trait ParCrf[T <: DenseCrf] extends DenseCrf {

  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) : T
  
  implicit val ec = ExecutionContext.Implicits.global
  
  protected def getGradient(numProcesses: Int, seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = {
    val accessors = seqAccessor.splitAccessor(numProcesses).toVector
    def subFn(accessor: AccessSeq[AbstractInstance]) = {
      val crf = getWorker(lambdas,nls,nfs,segSize,gPrior)
      val localLL = crf.getGradient(false,accessor)
      (crf.gradient,localLL)
    }
    var logLi = (regularize())
    val results = accessors.par map subFn 
    val subLLs = results map {(r: Tuple2[Array[Double],Option[Double]]) =>
      r match {
        case (grad,Some(ll)) =>
          var i = 0
          while (i < nfs) { gradient(i) += grad(i) ; i += 1}
          ll
        case _ => throw new RuntimeException("Unexpected return values from Expectation Worker")
      }
    }
    Some(subLLs.foldLeft(0.0)(_+_) + logLi)           
  }
}

class DenseParallelCrf(numPs: Int, nls: Int, nfs: Int, segSize: Int, gPrior: Double) extends DenseCrf(nls,nfs,segSize,gPrior)
with ParCrf[DenseCrfWorker] with CondLogLikelihoodLearner[AbstractInstance] {
  
  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) = {
    new DenseCrfWorker(lambdas,nls,nfs,segSize,gPrior)
  }

  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = getGradient(numPs,seqAccessor)
}

class NeuralDenseParallelCrf(numPs: Int,
			     nls: Int, 
			     nfs: Int, 
			     segSize: Int, 
			     opts: Options,
			     nNfs: Int = 0,
			     nGates: Int = 2) 
extends NeuralDenseCrf(nls,nfs,segSize,opts,nNfs,nGates)
with ParCrf[NeuralDenseCrfWorker] with CondLogLikelihoodLearner[AbstractInstance] {

  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) = {
    new NeuralDenseCrfWorker(lambdas, nls, nfs, ss, gPrior, nNfs, nGates, opts)
  }

  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = getGradient(numPs,seqAccessor)
}
