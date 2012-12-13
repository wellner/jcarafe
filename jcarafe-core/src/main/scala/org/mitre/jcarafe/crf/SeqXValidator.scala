package org.mitre.jcarafe.crf

trait SeqXValidator {
  
  def xValidate(crf: Crf, accessSeq: MemoryAccessSeq, num: Int = 200, folds: Int = 10): Unit = {
    val trainingTestSplits = accessSeq.getAllSplits(folds)
    var nll = 0.0
    trainingTestSplits foreach { case(tr, tst) =>
      crf.resetParameters()
      val cm = crf.train(tr, num)
      val cr = new DenseCRFConfidences(cm) with CondLogLikelihoodLearner[AbstractInstance]
      tst.getSeqs foreach {s => if (s.length > 0) nll -= cr.gradOfSeq(s.iseq)}
      }
    println("XValidation complete\n\n")
    println("NCLL = " + nll)
  }

}