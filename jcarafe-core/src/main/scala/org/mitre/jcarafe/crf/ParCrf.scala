/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.Options

trait DenseWorker extends DenseCrf {
  override def train(a:AccessSeq) = throw new RuntimeException("Class doesn't support training")
  override def train(a:AccessSeq, x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None) = throw new RuntimeException("Class doesn't support training")
}

class DenseCrfWorker(lambdas: Array[Double], nls: Int,nfs: Int,segSize: Int,gPrior: Double) 
extends DenseCrf(lambdas,nls,nfs,segSize,gPrior,0,0) with DenseWorker


class NeuralDenseCrfWorker(lambdas: Array[Double], nls: Int,nfs: Int,segSize: Int,gPrior: Double, nNfs: Int, nGates: Int, opts: Options) 
extends NeuralDenseCrf(lambdas, nls, nfs, segSize, opts, nNfs,nGates) with DenseWorker


trait ParCrf[T <: DenseCrf] extends DenseCrf {

  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) : T
  
  class Mapper[A, B: ClassManifest](l: Seq[A], f: A => B) {
    def pmap = {
      val buffer = new Array[B](l.length)
      val mappers =
        for(idx <- (0 until l.length).toList) yield {
          scala.actors.Futures.future {
            buffer(idx) = f(l(idx))
          }
        }
      for(mapper <- mappers) mapper()
      buffer
    }
  }

  protected def getGradient(numProcesses: Int, seqAccessor: AccessSeq) : Option[Double] = {
    val accessors = seqAccessor.splitAccessor(numProcesses).toArray
    val returns = new Mapper(accessors,{accessor: AccessSeq =>
      val crf = getWorker(lambdas,nls,nfs,segSize,gPrior)
      val localLL = crf.getGradient(false,accessor)
      (crf.gradient,localLL)
    })
    var logLi = (regularize())
    val subLLs = returns.pmap map {(r: Tuple2[Array[Double],Option[Double]]) =>
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
with ParCrf[DenseCrfWorker] with CondLogLikelihoodLearner {
  
  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) = {
    new DenseCrfWorker(lambdas,nls,nfs,segSize,gPrior)
  }

  override def getGradient(seqAccessor: AccessSeq) : Option[Double] = getGradient(numPs,seqAccessor)
}

class NeuralDenseParallelCrf(numPs: Int,
			     nls: Int, 
			     nfs: Int, 
			     segSize: Int, 
			     opts: Options,
			     nNfs: Int = 0,
			     nGates: Int = 2) 
extends NeuralDenseCrf(nls,nfs,segSize,opts,nNfs,nGates)
with ParCrf[NeuralDenseCrfWorker] with CondLogLikelihoodLearner {

  def getWorker(lambdas: Array[Double],nls: Int, nfs: Int, ss: Int, gPrior: Double) = {
    new NeuralDenseCrfWorker(lambdas, nls, nfs, ss, gPrior, nNfs, nGates, opts)
  }

  override def getGradient(seqAccessor: AccessSeq) : Option[Double] = getGradient(numPs,seqAccessor)
}
