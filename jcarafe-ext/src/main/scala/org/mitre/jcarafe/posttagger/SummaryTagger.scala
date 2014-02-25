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

class SummaryTagger(argv: Array[String]) extends TaggerTask[Array[String]](argv) {

  import StandardSerializer._
  
  lazy val trainer = new FactoredTrainer[Array[String]](opts) {

    val fspecStr = FeatureManagerBuilder.getFeatureSpecString(opts.featureSpec.get)
    val builder = new PostFeatureManagerBuilder(fspecStr)
    
    //FeatureManager.setWordProperties(opts, mgr)
    //FeatureManager.setLexicon(opts,mgr)
    val mgr = builder.getFeatureManager
    val fr = new TrainingFactoredFeatureRep[Array[String]](mgr, opts)
    val sGen : TrSeqGen = new TrainingSeqGen[Array[String]] (fr, opts) with SummarizationTextSeqGen
  }	
  
  lazy val decoder = new FactoredDecoder[Array[String]](opts) {
    val model = readModel(opts.model.get) 
    val builder = new PostFeatureManagerBuilder(model.fspec)
    //mgr.lex_=(model.lex)
    val mgr = builder.getFeatureManager
    val fr = new DecodingFactoredFeatureRep[Array[String]](mgr, opts, model)
    opts.priorAdjust match {case Some(v) => model.adjustParameter(org.mitre.jcarafe.crf.IncrementalMurmurHash.hash(":U:",0),Label("Post",Map("summary" -> "no")),v) case None => }
    val sGen : FactoredDecodingSeqGen[Array[String]] =
      new FactoredDecodingSeqGen[Array[String]] (fr, model,opts) with SummarizationTextSeqGen with SeqGenScorer[Array[String]]
    setDecoder(true)
  }
}

object SummaryTaggerMain {
  def printUsage = println(" Usage: ")
  
  def main(iargv: Array[String]) : Unit = {
    val tagger = new SummaryTagger(iargv) 
    tagger.process()}
}
