package org.mitre.jcarafe.dparser

import org.scalatest.Spec
import org.mitre.jcarafe.crf.AbstractInstance

class TestMstInference extends Spec {
  
  def processInputGraph(sm: Array[Array[Double]]) = {
    val mstCrf = new org.mitre.jcarafe.dparser.ProjectiveMstCrf(10,1.0) with org.mitre.jcarafe.crf.CondLogLikelihoodLearner[AbstractInstance]
      val betas = mstCrf.getInsideProbabilities(sm)
      val alphas = mstCrf.getOutsideProbabilities(sm,betas)
      val ln = sm.length
      for (i <- 1 until ln) {
        var sc = 0.0
        for (j <- 0 until ln) {
          if (j != i) {
            if (j < i) {
              sc += math.exp(alphas(j)(i)(0) + betas(j)(i)(0))
            } else {
              sc += math.exp(alphas(i)(j)(1) + betas(i)(j)(1))
            }
          }
        }
        assert(((sc / math.exp(betas(0)(ln-1)(2))) - 1.0).abs < 1E-3) // assert these are all close to 1.0
      }
  }
  
  describe("Mst Marginal Inference") {
    it("marginal probabilities for each incomming edge to any word should sum to 1.0") {
      val sm = Array(Array(0.0,0.0,0.0,0.0), Array(2.0,0.0,30.0,0.0), Array(10.0,20.0,0.0,5.0), Array(0.0,3.0,30.0,0.0))
      val sm1 = Array(Array(0.0,0.0,0.0,0.0), Array(0.0,0.0,1.0,0.0), Array(1.0,1.0,0.0,0.0), Array(0.0,1.0,1.0,0.0))
      val sm2 = Array(Array(0.0,0.0,0.0,0.0), Array(2.0,0.0,30.0,6.0), Array(10.0,20.0,0.0,5.0), Array(4.0,3.0,30.0,0.0))
      val sm4 = Array(Array(0.0,0.0,0.0,0.0,0.0), Array(2.0,0.0,3.0,6.0,2.0), Array(3.0,2.0,0.0,0.5,1.2), Array(4.0,3.0,3.0,0.0,2.1), Array(0.8,1.0,1.2,1.0,0.0))
      val sm5 = Array(Array(0.0,0.0,0.0,0.0,0.0), Array(2.0,0.0,2.0,2.0,2.0), Array(2.0,2.0,0.0,2.0,2.0), Array(2.0,2.0,2.0,0.0,2.0), Array(2.0,2.0,2.0,2.0,0.0))
      val sm6 = Array(Array(0.0,0.0,0.0,0.0,0.0,0.0), Array(1.0,0.0,1.0,1.0,1.0,1.0), Array(1.0,1.0,0.0,1.0,1.0,1.0), Array(1.0,1.0,1.0,0.0,1.0,1.0), Array(1.0,1.0,1.0,1.0,0.0,1.0),
          Array(1.0,1.0,1.0,1.0,1.0,1.0,0.0))
      val sm3 = 
        Array(Array(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0), 
        	  Array(2.0,0.0,30.0,0.0,2.0,4.0,10.0,2.0), 
        	  Array(10.0,20.0,0.0,5.0,3.0,2.0,5.0,2.0), 
        	  Array(0.0,3.0,30.0,0.0,9.0,12.0,0.0,2.0),
        	  Array(3.0,5.0,1.0,4.0,0.0,2.0,1.0,5.0),
        	  Array(4.0,1.0,3.4,2.4,4.9,0.0,1.2,4.2),
        	  Array(7.2,2.0,3.2,3.3,4.2,4.9,0.0,2.1),
        	  Array(2.0,3.1,2.1,3.5,2.1,5.6,0.0,0.0))
      //processInputGraph(sm)
      //processInputGraph(sm1)        	  
      //processInputGraph(sm2)
      //processInputGraph(sm4)
      //processInputGraph(sm5)
      //processInputGraph(sm6)
      processInputGraph(sm6)  	  
      processInputGraph(sm3)
    }
  }
  
  /*
  describe("Projective Training") {
    it("Training should finish, correctly minimizing conditional negative log-likelihood") {
      val tagger = new ProjectiveDependencyParser(Array("--input-file","/home/wellner/Projects/DepParsing/parse1.conll","--model","/home/wellner/Projects/DepParsing/model1",
          "--non-factored-fspec","/home/wellner/Projects/DepParsing/parserSpec0.fspec","--train", "--max-iters", "60", "--num-states","100","--gaussian-prior","1E300"))
      tagger.process()
    }
  }
  
  describe("Projective Training with PSA") {
    it("Training should finish ....") {
      val tagger = new ProjectiveDependencyParser(Array("--input-file","/home/wellner/Projects/DepParsing/parse1.conll","--model","/home/wellner/Projects/DepParsing/model1",
          "--non-factored-fspec","/home/wellner/Projects/DepParsing/parserSpec0.fspec","--train", "--max-iters", "10", "--num-states","100","--gaussian-prior","1E300", "--psa"))
      tagger.process()
    }
  }
  */
}