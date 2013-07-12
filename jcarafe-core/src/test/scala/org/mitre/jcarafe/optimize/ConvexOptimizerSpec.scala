package org.mitre.jcarafe.optimize

import org.scalatest.Spec

class ConvexOptimizerSpec extends Spec {

  describe("Optimizer test 1") {
    it("Mean test") {
      val target = 3.0
      val fn = new FunctionEvaluation {
        def evaluate(x: Array[Double], gradient: Array[Double], n: Int, step: Double) : Double = {
          println("x[0] = " + x(0))
          val resid = x(0) - target
          val sqloss = math.pow(resid,2.0)
          gradient(0) = resid
          sqloss
        }
      }
      val p = new Params
      val sol = Array(0.0)
      val optimizer = new LbfgsOptimizer(sol, fn, p)
      optimizer.optimize()
      println("-- Mean Test Solution: " + sol(0))
    }
  }
}