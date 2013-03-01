package org.mitre.jcarafe.crf

trait SeqXValidator {
  
  def xValidate[Obs](sGen: SeqGen[Obs], crf: Crf, accessSeq: MemoryAccessSeq, num: Int = 200, folds: Int = 10): Unit = {
    val trainingTestSplits = accessSeq.getAllSplits(folds)
    var nll = 0.0
    trainingTestSplits foreach { case(tr, tst) =>
      crf.resetParameters()
      val cm = crf.train(tr, num)
      val cr = new DenseCRFConfidences(cm, true) with CondLogLikelihoodLearner[AbstractInstance]
      val decoder = new Viterbi(cm)
      println("Evaluating fold...")
      tst.getSeqs foreach {s => if (s.length > 0) nll -= cr.gradOfSeq(s.iseq); decoder.assignBestSequence(s)}
      sGen.evaluateSequences(tst.getSeqs) // evaluate (stores numbers in sGen
      tst.getSeqs foreach {s => s.iseq foreach {el => el.label_=(el.orig)}} // need to reset these for subseqeutn training folds
      }
    val acc = sGen.getAccuracy
    println("XValidation complete\n\n")
    println("Negative Conditional Log-likelihood = " + nll)
    println("Sequence element accuracy = " + acc)
  }

}