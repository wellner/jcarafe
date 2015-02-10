/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import org.mitre.jcarafe.tagger.TaggerTask
import org.mitre.jcarafe.crf.FactoredTrainer
import org.mitre.jcarafe.crf.FactoredDecoder
import org.mitre.jcarafe.crf.TrainingSeqGen
import org.mitre.jcarafe.crf.FactoredDecodingSeqGen
import org.mitre.jcarafe.crf.TextSeqGen
import org.mitre.jcarafe.crf.SeqGenScorer
import org.mitre.jcarafe.crf.TrainingFactoredFeatureRep
import org.mitre.jcarafe.crf.DecodingFactoredFeatureRep
import org.mitre.jcarafe.crf.FeatureManagerBuilder
import org.mitre.jcarafe.crf.SeqGenScorer
import org.mitre.jcarafe.crf.StandardSerializer
import org.mitre.jcarafe.util._

class DATagger(argv: Array[String]) extends TaggerTask[Array[String]](argv) {

  import StandardSerializer._
  
  lazy val trainer = new FactoredTrainer[Array[String]](opts) {

    val fspecStr = FeatureManagerBuilder.getFeatureSpecString(opts.featureSpec.get)
    val builder = new PostFeatureManagerBuilder(fspecStr)
    val mgr = builder.getFeatureManager
    //FeatureManager.setLexicon(opts,mgr)
    val fr = new TrainingFactoredFeatureRep[Array[String]](mgr, opts)
	val sGen : TrSeqGen = new TrainingSeqGen[Array[String]] (fr, opts) with DATextSeqGen
  }	
  
  lazy val decoder = new FactoredDecoder[Array[String]](opts) {
    val model = readModel(opts.model.get) 
    val builder = new PostFeatureManagerBuilder(model.fspec)
    val mgr = builder.getFeatureManager
    //mgr.lex_=(model.lex)
      val fr = new DecodingFactoredFeatureRep[Array[String]](mgr, opts, model, false)
	  val sGen : FactoredDecodingSeqGen[Array[String]] =
		  new FactoredDecodingSeqGen[Array[String]] (fr, model,opts) with DATextSeqGen with SeqGenScorer[Array[String]]
      setDecoder(true)
  	}
}

object DATaggerMain {
  def printUsage = println(" Usage: ")
  
  def main(iargv: Array[String]) : Unit = {
    val argv = Array("text") ++ iargv // just add that this is "text" mode here 
    if (argv.length < 2) printUsage
    else if ((argv.length > 1) && 
    		(argv(1) == "train" || argv(1) == "test" || argv(1) == "decode" || argv(1) == "evaluate")) {
      val tagger = new DATagger(argv) 
      tagger.process()}
    else printUsage
    }
}
