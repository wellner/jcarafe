package org.mitre.jcarafe.optimize

import org.scalatest.Spec

class ConvexOptimizerSpec extends Spec {

  describe("Optimizer test 1") {
    it("Quadratic test") {
      val t = 3
      val fn = new FunctionEvaluation {
        def evaluate(x: Array[Double], gradient: Array[Double], n: Int, step: Double) : Double = {
          println("x[0] = " + x(0))          
          val res = x(0) - t
          val loss = res * res
          gradient(0) = res
          loss
        }
      }
      val p = new Params
      val sol = Array(0.0)
      val optimizer = new LbfgsOptimizer(sol, fn, p)
      val res = optimizer.optimize()
      println("-- Result: " + res)
      println("-- Mean Test Solution: " + sol(0))
      
    }
  }
}