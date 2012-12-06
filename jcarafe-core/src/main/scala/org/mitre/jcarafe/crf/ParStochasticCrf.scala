/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import org.mitre.jcarafe.util.Options
import scala.actors._

case class SectionUpdate(lambdas: Array[Double], etas: Array[Double]) 

class PsaWorker[T <: StochasticCrf](val estimator: T) extends Actor {
  def act() = {
    Actor.loop {
      react {
	case accessSeq: AccessSeq[AbstractInstance] =>
	  val core = estimator.train(accessSeq) // "train", just a single pass
	  reply(SectionUpdate(core.params,estimator.etas)) // return parameters
	case newParams: SectionUpdate =>
	  estimator.setNewParams(newParams.lambdas)
	  reply()
	case _ => 
	  exit()
      }
    }
  }
}


trait ParallelStochastic[T <: StochasticCrf] extends StochasticCrf {

  override val quiet = true

  def getWorker : T

  val numWorkers : Int = opts.numThreads match {
    case Some(n) => n.toInt 
    case None => 
      val n = Runtime.getRuntime.availableProcessors * 4/5
      if (n > 2) n else 1} // if we only get a couple of threads, just use a single thread to avoid overhead

  val psaWorkers : Vector[Actor] = Vector.tabulate(numWorkers){_ => 
    new PsaWorker(getWorker).start}

  override def train(accessSeq: AccessSeq[AbstractInstance], x: Int, modelIterFn: Option[(CoreModel,Int) => Unit] = None) = {
    println("\nConcurrent Stochastic Gradient Descent Training (with PSA) over " + accessSeq.length + " sequences")
    println("\t The eta's are initialized to " + etas(0) + " [val etas = Array.fill(nfs)(initialLearningRate)]")
    val accessors : IndexedSeq[AccessSeq[AbstractInstance]] = accessSeq.splitAccessor(numWorkers).toIndexedSeq
    var t = 0
    val newLambdas = Array.fill(nfs)(0.0)
    val newEtas = Array.fill(nfs)(1.0)
    while (t < maxEpochs) {
      val futures  = for ((f,i) <- accessors.zipWithIndex) yield psaWorkers(i) !! accessors(i)
      assign(newLambdas,{_ => 0.0})
      assign(newEtas,{_ => 1.0})
      futures.zipWithIndex foreach {case (f,i) => 
	val worker = psaWorkers(i)
	f() match {
	  case SectionUpdate(ls, etas) => assign1(newLambdas, ls, (_+_))
	  case _ => throw new RuntimeException("Unexepcted actor return value")}}
      if (numWorkers > 1) assign(newLambdas, (_ / numWorkers))
      psaWorkers foreach {_ ! SectionUpdate(newLambdas,newEtas)} 
      t += 1
    }
    psaWorkers foreach { _ ! None}
    new CoreModel(newLambdas, nfs, nls)
  }

}


class ParStochasticCrf(nls: Int, nfs: Int, segSize: Int, opts: Options) extends StochasticCrf(nls, nfs, segSize, opts) with ParallelStochastic[StochasticCrf] {

  def getWorker : StochasticCrf = {
    val nOpts = opts.copy()
    nOpts.maxIters = 1
    nOpts.gaussian = Double.MaxValue
    nOpts.batchSize = 1
    nOpts.CValue = 0.1
    new StochasticCrf(nls, nfs, segSize, nOpts) with PsaLearner[AbstractInstance]
  }
}
