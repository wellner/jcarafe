/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.semisupervised

import org.mitre.jcarafe.tagger.StdTaggerTask
import org.mitre.jcarafe.crf.{StdDecoder,SelfInducibleDecodingFactoredFeatureRep,FactoredDecodingSeqGen,
				  TextSeqGen,TextSeqDeserialization,InducedFeatureMap}
import org.mitre.jcarafe.tokenizer.FastTokenizer
import org.mitre.jcarafe.util.Options

class PosteriorTaggerTask(opts: Options) extends StdTaggerTask(opts) {

  def this(argv: Array[String]) = this(new Options(argv))

  override def getDecoder(modelFile: String, eval: Boolean, preModel: Boolean = false) : StdDecoder = {
    val decoder = new PosteriorDecoder(opts,modelFile) {
      val selfInducibleFRep = new SelfInducibleDecodingFactoredFeatureRep[String](opts,model)
      val sGen = new FactoredDecodingSeqGen[String](selfInducibleFRep,model,opts) with TextSeqGen {
	override def deserializeFromFile(f: String) = new TextSeqDeserialization(FastTokenizer.parseFileNoTags(f))
      }
    }
    decoder.setDecoder(true)
    decoder
  }

}

object PosteriorTaggerTaskMain {
  def main(argv: Array[String]) = {
    val t = new PosteriorTaggerTask(argv)
    org.mitre.jcarafe.crf.CrfInstance.useCache = false
    t.process()
  }
}
