/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import org.mitre.jcarafe.tagger.TaggerTask
import org.mitre.jcarafe.crf.{NonFactoredTrainer, NonFactoredDecoder, TrainingSeqGen, DecodingSeqGen, TextSeqGen, SeqGenScorer,
				  TrainingFactoredFeatureRep, NonFactoredFeatureRep, DecodingNonFactoredFeatureRep, 
				  DecodingFactoredFeatureRep, FeatureManager, NonFactoredFeatureManager, NonFactoredTrainingSeqGen,
				  NonFactoredDecodingSeqGen, NonFactoredPreFeature, SourceSequence, ObsSource, NonFactoredSerializer}

class AntecedentParser(argv: Array[String]) extends TaggerTask[Array[PostTok]](argv) {

  import NonFactoredSerializer._

  lazy val trainer = new NonFactoredTrainer[Array[PostTok]](opts) {
    var maxBack = opts.numStates match {case Some(v) => v - 1 case None => 2 }
    val mgr = DynamicAntecedentFeatureManager(maxBack, opts.nonFactoredFeatureSpec.get)
    val fr = new NonFactoredFeatureRep[Array[PostTok]](mgr, maxBack)
	val sGen = new NonFactoredTrainingSeqGen[Array[PostTok]] (fr, opts) with AntecedentSeqGen 
  }

  val postJsonTrainer = new NonFactoredTrainer[Array[PostTok]](opts) {
    var maxBack = opts.numStates match {case Some(v) => v - 1 case None => 2 }
    val mgr = DynamicAntecedentFeatureManager(maxBack, opts.nonFactoredFeatureSpec.get)
    val fr = new NonFactoredFeatureRep[Array[PostTok]](mgr, maxBack)
	val sGen = new NonFactoredTrainingSeqGen[Array[PostTok]] (fr, opts) with PostJsonSeqGen 
  }
  
  lazy val decoder = new NonFactoredDecoder[Array[PostTok]] {
    var maxBack = opts.numStates match {case Some(v) => v - 1 case None => 2 }
    val model = readModel(opts.model.get)
    val mgr = new DynamicAntecedentFeatureManager(maxBack, model.fspec)
    val fm = model.faMap
    fm.fixed_=(true)
    maxBack = model.crf.nls - 1
    val fr = new DecodingNonFactoredFeatureRep[Array[PostTok]](fm,mgr,maxBack)
    val sGen = new NonFactoredDecodingSeqGen[Array[PostTok]] (fr, model, opts) with AntecedentSeqGen 
  }	    
}

object AntecedentMain {
  def printUsage = println(" Usage: ")
  
  def main(iargv: Array[String]) : Unit = {
    val argv = Array("text") ++ iargv // just add that this is "text" mode here

    if (argv.length < 2) printUsage
    else if ((argv.length > 1) && 
    		(argv(1) == "train" || argv(1) == "test" || argv(1) == "decode" || argv(1) == "evaluate")) {
      val tagger = new AntecedentParser(argv) 
      tagger.process()}
    else printUsage
  }
}	
