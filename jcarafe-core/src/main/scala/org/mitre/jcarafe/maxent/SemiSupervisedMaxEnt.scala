/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.maxent
import org.mitre.jcarafe.util.{Options,OptionHandler}
import org.mitre.jcarafe.crf.{InducedFeatureMap}

class SemiSupervisedMaxEntClassifier(argv: Array[String]) extends MaxEntClassifier(argv) {

  import org.mitre.jcarafe.crf.MaxEntSerializer._

  override def process(printVecs: Boolean = false) = {
    val firstOpts : MEOptions = new MEOptions(argv, new MaxEntOptionHandler(argv))
    val decodeOpts : MEOptions = firstOpts.copy()
    
    decodeOpts.train = false
    decodeOpts.inputDir = firstOpts.unlabeledInputDir
    var inducedMap : Option[collection.mutable.HashMap[Long,Array[Double]]] = None
    val me = new MaxEntClassifier(firstOpts)
    me.process()
    var prevModel : Option[String] = None
    for (i <- 0 until firstOpts.selfInducedIterations) {
      val curDecodingOpts = decodeOpts.copy()
      prevModel match {case Some(s) => curDecodingOpts.model = Some(s) case None => }
      curDecodingOpts.train = false
      curDecodingOpts.inputDir = firstOpts.unlabeledInputDir
      println("Running decoder with model file: " + curDecodingOpts.model.getOrElse("NONE"))
      val decoder = new PosteriorMaxEntDecoder(curDecodingOpts, readModel(new java.io.File(curDecodingOpts.model.get)))
      decoder.decode()
      val newModelName = firstOpts.model map {n => n + ".SS" + i.toString} 
      prevModel = newModelName
      val curOpts : MEOptions = firstOpts.copy()
      curOpts.model = newModelName
      println("About to train to model: " + newModelName)
      val stdTagger : MaxEntClassifier = new MaxEntClassifier(curOpts, Some(InducedFeatureMap(decoder.faWeightMap)))
      val pr = (i == firstOpts.selfInducedIterations - 1)
      stdTagger.process(pr)
    }
  }
}

