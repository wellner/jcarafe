/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent
import org.mitre.jcarafe.util.Options

object ME {
  def main(argv: Array[String]) : Unit = {
    val opts = new MEOptions(argv, new MaxEntOptionHandler(argv))
    opts.tokenizerPatterns foreach { f => // set-up any special tokenization patterns, for file-based processing
      org.mitre.jcarafe.tokenizer.FastTokenizer.setTokenizerAugmenters(new java.io.File(f))
    }
    val me = 
      if (opts.selfInducedIterations > 0) new SemiSupervisedMaxEntClassifier(argv)
      else MaxEntClassifier(argv)
    me.process()
  }
}
