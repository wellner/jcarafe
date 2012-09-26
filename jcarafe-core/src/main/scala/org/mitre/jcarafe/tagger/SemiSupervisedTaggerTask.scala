/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.tagger

import org.mitre.jcarafe.util.{Options,OptionHandler}
import org.mitre.jcarafe.semisupervised.PosteriorTaggerTask

class SemiSupervisedTaggerTask(val origOptions: Options) extends StdTaggerTask(origOptions) {
  def this(argv: Array[String]) = this(new Options(argv))

  override def process() = {
    val firstTrainingOptions = origOptions.copy()
    firstTrainingOptions.weightedFeatureMap = None

    val decodingOpts = origOptions.copy()
    decodingOpts.train = false
    decodingOpts.inputDir = origOptions.unlabeledInputDir
 
    var inducedMap : Option[collection.mutable.HashMap[Long,Array[Double]]] = None
    val stdTagger = new StdTaggerTask(firstTrainingOptions)
    stdTagger.process() // initial training run
    var prevModel : Option[String] = None

    for (i <- 0 until origOptions.selfInducedIterations) {
      val curDecodingOpts = decodingOpts.copy()
      prevModel match {case Some(s) => curDecodingOpts.model = Some(s) case None => }
      curDecodingOpts.train = false
      curDecodingOpts.inputDir = origOptions.unlabeledInputDir
      println("Running decoder with model file: " + curDecodingOpts.model.getOrElse("NONE"))
      val posteriorTagger = new PosteriorTaggerTask(curDecodingOpts)
      org.mitre.jcarafe.crf.CrfInstance.useCache = false
      posteriorTagger.process()
      val newModelName = origOptions.model map {n => n + ".SS" + i.toString}
      prevModel = newModelName
      val curOpts = origOptions.copy()
      curOpts.model = newModelName
      val stdTagger = new StdTaggerTask(curOpts)
      stdTagger.process()
    }
    
  }

}
