package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{ CoreModel, DecodingAlgorithm, DenseTrainable, AbstractInstance, Feature, AccessSeq, SparseTrainable, CondLogLikelihoodLearner, NonFactoredTrainingSeqGen,
  LongAlphabet, NonFactoredModel}
import org.mitre.jcarafe.util.Options
import collection.mutable.HashMap

abstract class ProjectiveMst {

  @inline
  protected final def logSumExp(v1: Double, v2: Double) = {
    val vmin = math.min(v1, v2)
    val vmax = math.max(v1, v2)
    vmax + (math.log(math.exp(vmin - vmax) + 1))
  }

  private def sumR(s: Int, t: Int, sMat: Array[Array[Double]], ch: Array[Array[Array[Double]]]) = {
    val ch_s_t = ch(s)(t)
    var r = s; while (r <= t) {
      val ch_sr = ch(s)(r)
      val ch_rt = ch(r)(t)
      if (r < t) {
        val ch_r1t = ch(r + 1)(t)
        val v1 = ch_sr(2) + ch_r1t(3) + sMat(s)(t)
        ch_s_t(1) = logSumExp(ch_s_t(1), v1)
        val v2 = ch_sr(2) + ch_r1t(3) + sMat(t)(s)
        ch_s_t(0) = logSumExp(ch_s_t(0), v2)
      }
      r += 1
    }
    r = s + 1; while (r <= t) {
      ch_s_t(2) = logSumExp(ch_s_t(2), (ch(s)(r)(0) + ch(r)(t)(2)))
      r += 1
    }
    r = s; while (r < t) {
      ch_s_t(3) = logSumExp(ch_s_t(3), (ch(s)(r)(3) + ch(r)(t)(1)))
      r += 1
    }
  }

  def getInsideProbabilities(sMat: Array[Array[Double]]) = {
    val ln = sMat.length
    val ch = Array.tabulate(ln) { _ => Array.tabulate(ln) { _ => Array.fill(4)(-Double.MaxValue) } }
    for (s <- 0 until ln; state <- 0 until 4) {
      ch(s)(s)(state) = 0.0
    }
    var k = 1; while (k < ln) {
      var s = 0; while (s < (ln - k)) {
        sumR(s, s + k, sMat, ch)
        s += 1
      }
      k += 1
    }
    ch
  }

  def getOutsideProbabilities(sMat: Array[Array[Double]], beta: Array[Array[Array[Double]]]) = {
    val ln = sMat.length
    val alpha = Array.tabulate(ln) { _ => Array.tabulate(ln) { _ => Array.fill(4)(-Double.MaxValue) } }
    alpha(0)(ln - 1)(2) = 0.0
    var s = 0; while (s < ln) {
      var t = ln - 1; while (t >= s) {
        var r = 0; while (r < s) {
          alpha(s)(t)(2) = logSumExp(alpha(s)(t)(2), (alpha(r)(t)(2) + beta(r)(s)(0)))
          r += 1
        }
        r = t + 1; while (r < ln) {
          alpha(s)(t)(2) = logSumExp(alpha(s)(t)(2), (alpha(s)(r)(0) + beta(t + 1)(r)(3) + sMat(r)(s)))
          alpha(s)(t)(2) = logSumExp(alpha(s)(t)(2), (alpha(s)(r)(1) + beta(t + 1)(r)(3) + sMat(s)(r)))
          r += 1
        }
        r = t; while (r < ln) {
          alpha(s)(t)(3) = logSumExp(alpha(s)(t)(3), (alpha(s)(r)(3) + beta(t)(r)(1)))
          r += 1
        }
        r = 0; while (r < s) {
          alpha(s)(t)(3) = logSumExp(alpha(s)(t)(3), (alpha(r)(t)(1) + beta(r)(s - 1)(2) + sMat(r)(t)))
          alpha(s)(t)(3) = logSumExp(alpha(s)(t)(3), (alpha(r)(t)(0) + beta(r)(s - 1)(2) + sMat(t)(r)))
          r += 1
        }
        r = t; while (r < ln) {
          alpha(s)(t)(0) = logSumExp(alpha(s)(t)(0), (alpha(s)(r)(2) + beta(t)(r)(2)))
          r += 1
        }
        r = 0; while (r <= s) {
          alpha(s)(t)(1) = logSumExp(alpha(s)(t)(1), (alpha(r)(t)(3) + beta(r)(s)(3)))
          r += 1
        }
        t -= 1
      }
      s += 1
    }
    alpha
  }
}

abstract class ProjectiveMstCrf(val nfs: Int, val gPrior: Double = 100.0) extends ProjectiveMst with DenseTrainable[AbstractInstance] {

  /**
   * These are the model parameters
   */
  val lambdas: Array[Double] = Array.fill(nfs)(0.0)
  val numParams = nfs
  val gradient: Array[Double] = Array.fill(nfs)(0.0)
  val featureExpectations = Array.fill(nfs)(0.0)
  val invSigSqr = 1.0 / gPrior

  def initialize() = {}
  def getCoreModel() = new CoreModel(lambdas, numParams, 0, 0, 0)
  

  def regularize() = {
    var i = 0
    var llMod = 0.0
    val llen = lambdas.length
    while (i < llen) {
      val li = lambdas(i)
      gradient(i) = li * invSigSqr
      llMod -= (li * li * invSigSqr) / 2.0
      i += 1
    }
    llMod
  }

  protected def inferGradientAndLL(iseq: Seq[AbstractInstance]) = {
    var sll = 0.0
    val sl = iseq.length
    val sMat = Array.fill(sl + 1, sl + 1)(0.0)

    var i = 0; while (i < sl) {
      val instFeatures = iseq(i).getCompVec(0)
      val label = iseq(i).label
      updateScores(sMat, instFeatures, lambdas, i, label)
      i += 1
    }
    i = 0; while (i < nfs) { featureExpectations(i) = -Double.MaxValue; i += 1 }
    val beta = getInsideProbabilities(sMat)
    val alpha = getOutsideProbabilities(sMat, beta)
    val logzx = beta(0)(sl)(2)
    i = 0; while (i < sl) {
      val instFeatures = iseq(i).getCompVec(0)
      val klen = instFeatures.length
      val label = iseq(i).label
      var k = 0; while (k < klen) {
        val inst = instFeatures(k)
        val parent = inst.cur
        if (parent == label) sll += lambdas(inst.fid) * inst.value
        val s = math.min(i, parent) + 1
        val t = math.max(i, parent) + 1
        val d = if (i > parent) 0 else 1

        val exUpdate =
          if (s == t) { // case where this is referring to root
            beta(0)(s)(0) + alpha(0)(s)(0)
          } else {
            beta(s)(t)(d) + alpha(s)(t)(d)
          }
        val cfid = inst.fid
        featureExpectations(cfid) = logSumExp(featureExpectations(cfid), exUpdate)
        k += 1
      }
      i += 1
    }
    i = 0; while (i < nfs) {
      gradient(i) += math.exp(featureExpectations(i) - logzx); i += 1
    }
    if (sll > logzx) {
      println("WARNING: sll - logzx > 0.0 !! => sll = " + sll + " logzx = " + logzx + " length = " + sl)
    }
    sll - logzx
  }

  protected def updateScores(scoreMat: Array[Array[Double]], instFeatures: Array[Feature], lambdas: Array[Double], pos: Int, lab: Int) = {
    val klen = instFeatures.length
    val apos = pos + 1 // to handle addition of "root"

    var k = 0; while (k < klen) {
      val inst = instFeatures(k)
      val parent = inst.cur
      if (parent == lab) {
        gradient(inst.fid) -= inst.value // constraint
      }
      if ((parent + 1) == apos) // this is a link from ROOT
        scoreMat(apos)(0) += lambdas(inst.fid) * inst.value
      else
        scoreMat(apos)(parent + 1) += lambdas(inst.fid) * inst.value
      k += 1
    }
  }

  def getGradient(li: Boolean, seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = {
    var logLi = if (li) (regularize()) else 0.0 // this also resets the gradient
    for (j <- 0 until seqAccessor.length) {
      val seq = seqAccessor(j)
      if (seq.length > 0) logLi += inferGradientAndLL(seq)
    }
    Some(-logLi)
  }

  def getGradient(seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = getGradient(true, seqAccessor)
}

abstract class StochasticProjectiveMstCrf(val nfs: Int, val opts: Options, modelIterFn: Option[String => Unit] = None) extends ProjectiveMst with SparseTrainable[AbstractInstance] {

  val quiet = false

  val lambdas: Array[Double] = Array.fill(nfs)(0.0)
  val numParams = nfs
  val gradient: HashMap[Int, DoubleCell] = new HashMap[Int, DoubleCell]()
  val periodSize = opts.psaPeriodSize
  val maxEpochs = opts.maxIters
  val batchSize = opts.batchSize
  val C = opts.CValue
  val initialLearningRate = opts.learningRate
  val momentum = opts.momentum
  val pAlpha = opts.pAlpha
  val invSigSqr = 1.0 / opts.gaussian
  def initialize() = {}
  def getCoreModel() = new CoreModel(lambdas, numParams, 0, 0, 0)

  var numGradIssues = 0
  lazy val etas = Array.fill(nfs)(initialLearningRate) // only used for PSA-based learning, hence its laziness
  val eta = initialLearningRate
  var curPos: Int = 0

  protected def updateScores(scoreMat: Array[Array[Double]], instFeatures: Array[Feature], lambdas: Array[Double], pos: Int, lab: Int) = {
    val klen = instFeatures.length
    val apos = pos + 1 // to handle addition of "root"

    var k = 0; while (k < klen) {
      val inst = instFeatures(k)
      val parent = inst.cur
      val gref = gradient.get(inst.fid) match {
        case Some(v) => if (parent == lab) v.g += inst.value
        case None =>
          val nv = new DoubleCell(0.0, -Double.MaxValue)
          if (parent == lab) nv.g += inst.value
          gradient.update(inst.fid, nv)
          nv
      }
      if ((parent + 1) == apos) // this is a link from ROOT
        scoreMat(apos)(0) += lambdas(inst.fid) * inst.value
      else
        scoreMat(apos)(parent + 1) += lambdas(inst.fid) * inst.value
      k += 1
    }
  }

  val gNormMax = 600.0

  def getGradient(seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = {
    val asize = batchSize min seqAccessor.length
    var gradNormalizer = 0.0
    var totalLL = 0.0
    for (k <- curPos until curPos + asize) {
      val j = k % seqAccessor.length
      val iseq = seqAccessor(j)
      val sl = iseq.length
      var sll = 0.0
      val sMat = Array.fill(sl + 1, sl + 1)(0.0)

      if (sl > 0) {
        var i = 0; while (i < sl) {
          val instFeatures = iseq(i).getCompVec(0)
          val label = iseq(i).label
          updateScores(sMat, instFeatures, lambdas, i, label)
          i += 1
        }
        gradient.foreach { case (k, v) => v.e_=(-Double.MaxValue) } // reset expectations to LOG-zero
        val betas = getInsideProbabilities(sMat)
        val alphas = getOutsideProbabilities(sMat, betas)
        val zx = betas(0)(sl)(2)
        i = 0; while (i < sl) {
          val instFeatures = iseq(i).getCompVec(0)
          val klen = instFeatures.length
          val label = iseq(i).label
          var k = 0; while (k < klen) {
            val inst = instFeatures(k)
            val parent = inst.cur
            if (parent == label) sll += lambdas(inst.fid) * inst.value // assume just binary features right now
            val s = math.min(i, parent) + 1
            val t = math.max(i, parent) + 1
            val d = if (i > parent) 0 else 1
            val exUpdate =
              if (s == t) { // case where this is referring to root
                betas(0)(s)(0) + alphas(0)(s)(0)
              } else {
                betas(s)(t)(d) + alphas(s)(t)(d)
              }
            val cfid = inst.fid
            val gref = gradient(cfid)
            gref.e = logSumExp(gref.e, exUpdate)
            k += 1
          }
          i += 1
        }
        gradient foreach {
          case (id, cell) =>
            val gv = math.exp(cell.e - zx)
            cell.g -= gv
            val cabs = math.abs(cell.g)
            if (cabs > gradNormalizer) { gradNormalizer = cabs }
        }
        if (sll > zx) println("WARNING: sll - logzx > 0.0 !! => sll = " + sll + " logzx = " + zx + " length = " + sl)
        totalLL += (sll - zx)
      }

    }

    curPos += asize
    if (gradNormalizer > gNormMax) {
      numGradIssues += 1
      val nn = gNormMax / gradNormalizer
      for ((k, cell) <- gradient) {
        cell.g_=((cell.g * nn) - lambdas(k) * invSigSqr)
      }
    } else {
      for ((k, cell) <- gradient) {
        cell.g_=(cell.g - lambdas(k) * invSigSqr)
      }
    }
    Some(totalLL)
  }

}

class ProjectiveMstInference(crf: CoreModel) extends DecodingAlgorithm(crf) {

  val lambdas = crf.params

  protected def updateScores(scoreVec: Array[Double], instFeatures: Array[Feature], lambdas: Array[Double], pos: Int) = {
    val apos = pos + 1
    val klen = instFeatures.length
    var k = 0; while (k < klen) {
      val inst = instFeatures(k)
      val parent = inst.cur
      if ((parent + 1) == apos) // this is a link from ROOT
        scoreVec(0) += lambdas(inst.fid) * inst.value
      else
        scoreVec(parent + 1) += lambdas(inst.fid) * inst.value
      k += 1
    }
  }

  def getCopyOf = new ProjectiveMstInference(this.crf)

  def assignBestSequence(iseq: Seq[AbstractInstance]): Double = {
    val sl = iseq.length
    //val sMat = Array.fill(sl + 1, sl + 1)(0.0)
    val sMat = Array.tabulate(sl + 1) { r => if (r == 0) Array.fill(sl + 1)(-Double.MaxValue) else Array.tabulate(sl + 1) { s => if (r == s) -Double.MaxValue else 0.0 } }
    val t0 = System.nanoTime
    var i = 0; while (i < sl) {
      val instFeatures = iseq(i).getCompVec(0)
      val label = iseq(i).label
      var sVec = sMat(i + 1)
      updateScores(sVec, instFeatures, lambdas, i)
      i += 1
    }
    val t1 = System.nanoTime
    val eisner = new EisnerInference()
    val t2 = System.nanoTime
    val bstParse =
      eisner.eisnerInfer(sMat) foreach {
        case Edge(src, sink) =>
          iseq(sink - 1).label_=(if (src == 0) sink - 1 else src - 1)
      } // these are -1 to map back to original offsets without ROOT
    0.0
  }
}

case class Edge(src: Int, sink: Int)

class ChartEntry(var score: Double, var edge: Option[Edge], var r: Int) {
  override def toString(): String = {
    "score: " + score + " edge: " + edge + " r = " + r
  }
}

class EisnerInference {

  /*
   * State mapping:
   * 0 - right, incomplete
   * 1 - left, incomplete
   * 2 - right, complete
   * 3 - left, complete 
   */

  def getChart(s: Int): Array[Array[Array[ChartEntry]]] = {
    Array.tabulate(s) { x =>
      Array.tabulate(s) { y => if (x == y) Array.fill(4)(new ChartEntry(0.0, None, 0)) else Array.fill(4)(new ChartEntry(-Double.MaxValue, None, 0)) }
    }
  }

  def maxR(s: Int, t: Int, sMat: Array[Array[Double]], ch: Array[Array[Array[ChartEntry]]]) = {
    val ch_s_t_1 = ch(s)(t)(1)
    val ch_s_t_0 = ch(s)(t)(0)
    val ch_s_t_3 = ch(s)(t)(3)
    val ch_s_t_2 = ch(s)(t)(2)
    var r = s; while (r < t) {
      val ch_sr = ch(s)(r)
      val ch_r1t = ch(r + 1)(t)
      val v1 = ch_sr(2).score + ch_r1t(3).score + sMat(s)(t)
      if (ch_s_t_1.score < v1) {
        ch_s_t_1.score = v1
        ch_s_t_1.edge = Some(Edge(t, s))
        ch_s_t_1.r = r
      }
      val v2 = ch_sr(2).score + ch_r1t(3).score + sMat(t)(s)
      if (ch_s_t_0.score < v2) {
        ch_s_t_0.score = v2
        ch_s_t_0.edge = Some(Edge(s, t))
        ch_s_t_0.r = r
      }
      r += 1
    }
    r = s; while (r <= t) {
      val ch_sr = ch(s)(r)
      val ch_rt = ch(r)(t)
      val v3 = ch_sr(3).score + ch_rt(1).score
      if (ch_s_t_3.score < v3) {
        ch_s_t_3.score = v3
        ch_s_t_3.r = r
      }
      val v4 = ch(s)(r)(0).score + ch_rt(2).score
      if (ch_s_t_2.score < v4) {
        ch_s_t_2.score = v4
        ch_s_t_2.r = r
      }
      r += 1
    }
  }

  def unwindParse(curEntry: ChartEntry, s: Int, t: Int, state: Int, chart: Array[Array[Array[ChartEntry]]]): Set[Edge] = {
    if (t > s) {
      val r = curEntry.r
      if (state == 0 || state == 1) {
        val leftE = chart(s)(r)(2)
        val rightE = chart(r + 1)(t)(3)
        unwindParse(leftE, s, r, 2, chart) ++ unwindParse(rightE, r + 1, t, 3, chart) + curEntry.edge.get
      } else if ((state == 2)) {
        val leftE = chart(s)(r)(0)
        if (r < t) {
          val rightE = chart(r)(t)(2)
          unwindParse(leftE, s, r, 0, chart) ++ unwindParse(rightE, r, t, 2, chart)
        } else unwindParse(leftE, s, r, 0, chart)
      } else {
        val rightE = chart(r)(t)(1)
        if (s < r) {
          val leftE = chart(s)(r)(3)
          unwindParse(leftE, s, r, 3, chart) ++ unwindParse(rightE, r, t, 1, chart)
        } else {
          unwindParse(rightE, r, t, 1, chart)
        }
      }
    } else Set()
  }

  def eisnerInfer(sMat: Array[Array[Double]]) = {
    val ln = sMat.length
    val ch = getChart(ln)
    for (k <- 1 until ln) {
      for (s <- 0 until (ln - k)) {
        val t = s + k
        maxR(s, t, sMat, ch)
      }
    }
    var i = 0;
    val edgeSet = unwindParse(ch(0)(ln - 1)(2), 0, ln - 1, 2, ch)
    edgeSet
  }
}

trait ParMstCrf extends ProjectiveMstCrf {

  def getWorker(lambdas: Array[Double], nfs: Int, gPrior: Double): ProjectiveMstCrf

  class Mapper[A, B: ClassManifest](l: Seq[A], f: A => B) {
    def pmap = {
      val buffer = new Array[B](l.length)
      val mappers =
        for (idx <- (0 until l.length).toList) yield {
          scala.actors.Futures.future {
            buffer(idx) = f(l(idx))
          }
        }
      for (mapper <- mappers) mapper()
      buffer
    }
  }

  def getGradient(numProcesses: Int, seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = {
    val accessors = seqAccessor.splitAccessor(numProcesses).toArray
    val returns = new Mapper(accessors, { accessor: AccessSeq[AbstractInstance] =>
      val crf = getWorker(lambdas, nfs, gPrior)
      val localLL = crf.getGradient(false, accessor) // need to do the right thing with regularization
      (crf.gradient, localLL)
    })
    var logLi = regularize()
    val subLLs = returns.pmap map { (r: Tuple2[Array[Double], Option[Double]]) =>
      r match {
        case (grad, Some(ll)) =>
          var i = 0
          while (i < nfs) { gradient(i) += grad(i); i += 1 }
          ll
        case _ => throw new RuntimeException("Unexpected return values from Expectation Worker")
      }
    }
    Some(subLLs.foldLeft(0.0)(_ + _) - logLi)
  }
}

class ProjectiveMstCrfParallel(numPs: Int, nfs: Int, gPrior: Double = 100.0) extends ProjectiveMstCrf(nfs, gPrior) with ParMstCrf with CondLogLikelihoodLearner[AbstractInstance] {

  def getWorker(ls: Array[Double], nfs: Int, Prior: Double) = {
    new ProjectiveMstCrf(nfs, gPrior) {
      override val lambdas = ls
      def train(a: AccessSeq[AbstractInstance]) = throw new RuntimeException("Class doesn't support training")
      def train(a: AccessSeq[AbstractInstance], x: Int, mi: Option[(CoreModel,Int) => Unit]) = throw new RuntimeException("Class doesn't support training")
    }
  }

  override def getGradient(seqAccessor: AccessSeq[AbstractInstance]): Option[Double] = getGradient(numPs, seqAccessor)
}
