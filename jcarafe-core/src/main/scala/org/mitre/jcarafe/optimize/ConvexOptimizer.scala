package org.mitre.jcarafe.optimize

/*
 * The software contained within this file is a PORT of the L-BFGS convex 
 * optimization algorithm as implemented in the libLBFGS library: 
 * http://www.chokkan.org/software/liblbfgs/
 * 
 * The original FORTRAN implementation is available here:
 * http://users.eecs.northwestern.edu/~nocedal/lbfgs.html
 * 
 * This PORT from libBFGS (in C) to a derivative implementation in 
 * Scala was authored by Ben Wellner, The MITRE Corporation.
 * This software inherits the MIT License from libLBFGS:


 * The MIT License

Copyright (c) 1990 Jorge Nocedal
Copyright (c) 2007-2010 Naoaki Okazaki
Copyright (c) 2013 The MITRE Corporation

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 * 
 */

abstract class OptimizerStatus 
case object Success extends OptimizerStatus
case object Stopped extends OptimizerStatus
case object AlreadyMinimized extends OptimizerStatus
case object NotStarted extends OptimizerStatus
case object ErrUnknownerror extends OptimizerStatus
case object ErrLogicerror extends OptimizerStatus
case object ErrOutofmemory extends OptimizerStatus
case object ErrCanceled extends OptimizerStatus
case object ErrInvalidN extends OptimizerStatus
case object ErrInvalidNSse extends OptimizerStatus
case object ErrInvalidXSse extends OptimizerStatus
case object ErrInvalidEpsilon extends OptimizerStatus
case object ErrInvalidTestperiod extends OptimizerStatus
case object ErrInvalidDelta extends OptimizerStatus
case object ErrInvalidLinesearch extends OptimizerStatus
case object ErrInvalidMinstep extends OptimizerStatus
case object ErrInvalidMaxstep extends OptimizerStatus
case object ErrInvalidFtol extends OptimizerStatus
case object ErrInvalidWolfe extends OptimizerStatus
case object ErrInvalidGtol extends OptimizerStatus
case object ErrInvalidXtol extends OptimizerStatus
case object ErrInvalidMaxlinesearch extends OptimizerStatus
case object ErrInvalidOrthantwise extends OptimizerStatus
case object ErrInvalidOrthantwiseStart extends OptimizerStatus
case object ErrInvalidOrthantwiseEnd extends OptimizerStatus
case object ErrOutofinterval extends OptimizerStatus
case object ErrIncorrectTminmax extends OptimizerStatus
case object ErrRoundingError extends OptimizerStatus
case object ErrMinimumstep extends OptimizerStatus
case object ErrMaximumstep extends OptimizerStatus
case object ErrMaximumlinesearch extends OptimizerStatus
case object ErrMaximumIteration extends OptimizerStatus
case object ErrWidthtoosmall extends OptimizerStatus
case object ErrInvalidparameters extends OptimizerStatus
case object ErrIncreasegradient extends OptimizerStatus

object OptimizerStatus {
  def isError(e: OptimizerStatus) = !(e == Success || e == Stopped || e == AlreadyMinimized || e == NotStarted) 
}

abstract class LineSearchAlg
case object LineSearchBacktracking extends LineSearchAlg
case object LineSearchMoreThuente extends LineSearchAlg
case object LineSearchBacktrackingArmijo extends LineSearchAlg
case object LineSearchBacktrackingWolfe extends LineSearchAlg


class Params(
    val lineSearch : LineSearchAlg = LineSearchBacktracking,
    var m: Int = 6,
    var epsilon: Double = 1E-5,
    var past: Int = 3,
    var delta: Double = 1E-5,
    var maxIters: Int = 0, // value of '0' indicates iterate until convergence
    var maxLineSearch: Int = 40,
    var minStep : Double = 1E-20,
    var maxStep : Double = 1E20,
    var ftol : Double = 1E-4,
    var wolfe : Double = 0.9,
    var gtol : Double = 0.9,
    var xtol : Double = 1E-16,
    var verbose : Boolean = false
    )
    
case class Result(var status: OptimizerStatus, var additionalStatus: Int = 0, var objective: Double = 0.0, var gnorm: Double = 0.0,
    var numIterations: Int = 0)

class Cell[T](var v: T) {
  final def set(x: T) = v = x
  final def get = v
}

abstract class FunctionEvaluation {
  def evaluate(x: Array[Double], gradient: Array[Double], n: Int, step: Double) : Double
}

abstract class Numerical(val n: Int) {
  @inline
  final def vecCopyNegate(y: Array[Double], x: Array[Double]) = {
    var i = 0; while (i < n) {
      y(i) = -x(i)
      i += 1
    }
  }
  
  @inline
  final def vecCopy(y: Array[Double], x: Array[Double]) = {
    System.arraycopy(x, 0, y, 0, n)
  }
  
  @inline
  final def vecAdd(y: Array[Double], x: Array[Double], c: Double) = {
    var i = 0; while (i < n) {
      y(i) += c * x(i)
      i += 1
    }
  }
  
  @inline
  final def vecDiff(z: Array[Double], x: Array[Double], y: Array[Double]) = {
    var i = 0; while (i < n) {
      z(i) = x(i) - y(i)
      i += 1
    }
  }
  
  @inline
  final def vecScale(y: Array[Double], c: Double) = {
    var i = 0; while (i < n) {
      y(i) *= c
      i += 1
    }
  }
  
  @inline
  final def vecDot(y: Array[Double], x: Array[Double]) : Double = {
    var r = 0.0
    var i = 0; while (i < n) {
      r += y(i) * x(i)
      i += 1
    }
    r
  }
  
  @inline
  final def vec2norm(x: Array[Double]) = math.sqrt(vecDot(x, x))
  
  @inline
  final def vec2normInv(x: Array[Double]) = 1.0 / math.sqrt(vecDot(x, x))

}

abstract class ConvexOptimizer(n: Int) extends Numerical(n) {
  
  case class IterationData(var alpha: Double, val s: Array[Double], val y: Array[Double], var ys: Double)
  
  def optimize() : Result
  
}

 

class LbfgsOptimizer(val x: Array[Double], val g: Array[Double], val evaluator: FunctionEvaluation, val params: Params) extends ConvexOptimizer(x.length) {
  
  val lSearch : LineSearch = new BackTrackingLineSearch(x.length, evaluator, params)

  val xp       = Array.fill(n)(0.0)
  //val g        = Array.fill(n)(0.0)
  val gp       = Array.fill(n)(0.0)
  val d        = Array.fill(n)(0.0)
  val w        = Array.fill(n)(0.0)
  var step     = new Cell(0.0)  
  var improvementRate = 0.0
  val fx       = new Cell(0.0)
  var ys       = 0.0
  var yy       = 0.0
  
    
  var ret = Result(NotStarted)
  val lmStorage = Array.tabulate(params.m){_ => IterationData(0,Array.fill(n)(0.0),Array.fill(n)(0.0),0) }
  var curStor = IterationData(0,Array.fill(n)(0.0),Array.fill(n)(0.0),0)
  val pf = Array.fill(params.past)(0.0)
    
  private def printVec(g: Array[Double]) = {
    g foreach {e => print(" " + e)}
    println
  }
  
  def optimize() : Result = {
    
    fx.set(evaluator.evaluate(x, g, n, 0.0))
    pf(0) = fx.get
    vecCopyNegate(d, g)
    
    var xnorm = vec2norm(x)
    var gnorm = vec2norm(g)
    var k = 0
    var end = 0
    if (xnorm < 1.0) xnorm = 1.0
    if ((gnorm / xnorm) <= params.epsilon) return Result(AlreadyMinimized, gnorm = gnorm, objective = fx.get)
    step set vec2normInv(d)
    
    var continue = true
    while (continue) {
      vecCopy(xp, x)
      vecCopy(gp, g)
      val ls = lSearch.search(x, fx, g, d, step, xp, gp, w)
      if (OptimizerStatus.isError(ls)) { // revert to previous point and return
        vecCopy(x, xp)
        vecCopy(g, gp)
        return Result(ls,0,fx.get,gnorm,numIterations = k)
      }
      xnorm = vec2norm(x)
      gnorm = vec2norm(g)
      if (xnorm < 1.0) xnorm = 1.0
      if ((gnorm / xnorm) <= params.epsilon) {
        ret.status = Success
        continue = false
      }
      val ffx = fx.get
      if (params.verbose) {
        val k1 = k+1
        println(f"Obj = $ffx%10.6f  (Iter = $k1%d)")
      }
      if (continue && params.past <= k) {
        improvementRate = (pf(k % params.past) - ffx) / ffx
        if (improvementRate < params.delta) {
          ret.status = Stopped
          continue = false
        }
      }
      pf(k % params.past) = ffx
      
      if (continue && params.maxIters != 0 && params.maxIters < (k + 1)) {
        ret.status = ErrMaximumIteration
        continue = false
      }
      
      curStor = lmStorage(end)
      vecDiff(curStor.s, x, xp)
      vecDiff(curStor.y, g, gp)
      
      ys = vecDot(curStor.y, curStor.s)
      yy = vecDot(curStor.y, curStor.y)
      curStor.ys = ys
      val bound = if (params.m <= k) params.m else k
      k += 1
      end = (end + 1) % params.m
      vecCopyNegate(d, g)
      
      
      var j = end
      for (i <- 0 until bound) {
        j = (j + params.m - 1) % params.m
        curStor = lmStorage(j)
        curStor.alpha = vecDot(curStor.s, d)
        curStor.alpha /= curStor.ys
        vecAdd(d, curStor.y, -curStor.alpha)        
      }
      vecScale(d, ys / yy)
      var beta = 0.0
      for (i <- 0 until bound) {        
        curStor = lmStorage(j)
        beta = vecDot(curStor.y, d)
        beta /= curStor.ys
        vecAdd(d, curStor.s, curStor.alpha - beta)
        j = (j + 1) % params.m
      }
      
      step set 1.0
    } // end of BIG while loop
    ret.objective = fx.get
    ret.gnorm = gnorm
    ret.numIterations = k
    ret
  }
}

abstract class LineSearch(n: Int) extends Numerical(n) {
  def search(x: Array[Double],
    f: Cell[Double],
    g: Array[Double],
    s: Array[Double],
    stp: Cell[Double],
    xp: Array[Double],
    gp: Array[Double],
    wa: Array[Double]) : OptimizerStatus
}

class BackTrackingLineSearch(n: Int, val evaluator: FunctionEvaluation, val params: Params) extends LineSearch(n) {
  
  private def printVec(g: Array[Double]) = {
    g foreach {e => print(" " + e)}
    println
  }
  
  def search(x: Array[Double],
    f: Cell[Double],
    g: Array[Double],
    s: Array[Double],
    stp: Cell[Double],
    xp: Array[Double],
    gp: Array[Double],
    wa: Array[Double]) : OptimizerStatus = {
    
    var cnt = 0
    var dec = 0.5
    var dg = 0.0
    var inc = 2.1
    var width = 0.0
    var dgInit = 0.0
    var dgTest = 0.0
    var fInit = 0.0
    
    dgInit = vecDot(g, s)
    fInit = f.get
    dgTest = params.ftol * dgInit
    
    var continue = true
    assert(dgInit <= 0.0)
    while (continue) {
      vecCopy(x, xp)
      vecAdd(x, s, stp.get)
      f set evaluator.evaluate(x, g, n, stp.get)
      cnt += 1
      if (f.get > fInit + stp.get * dgTest) 
        width = dec
      else {
        if (params.lineSearch == LineSearchBacktrackingArmijo) {
          return Success
        } else {
          dg = vecDot(g, s)
          if (dg < (params.wolfe * dgInit)) width = inc
          else {
            if (params.lineSearch == LineSearchBacktrackingWolfe) return Success
            if (dg > (-params.wolfe * dgInit)) width = dec else return Success
            
          }
        }        
      }
      if (stp.get < params.minStep) return ErrMinimumstep
      if (stp.get > params.maxStep) return ErrMaximumstep
      if (params.maxLineSearch <= cnt) return ErrMaximumlinesearch
      stp set (stp.get * width)
    }    
    Success    
  }
}