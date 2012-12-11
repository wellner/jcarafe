/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.dparser
import org.mitre.jcarafe.tagger.TaggerTask
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util.Options


trait MstCrfTraining[O] extends Trainer[O] {
  
  def trainingRoutine(seqs: Seq[InstanceSequence]) = {
    println("Processed " + seqs.length + " sentences . . . beginning parameter estimation..\n")
    println("Number of features = " + sGen.getNumberOfFeatures)
    val mstCrf = new MstCrf(sGen.getNumberOfFeatures, opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
    trainModel(mstCrf,seqs)
  }
}

abstract class NonFactoredMstTrainer[O](adj: Boolean, o: Options) extends GenericNonFactoredTrainer[O](adj,o) with MstCrfTraining[O] 

class DependencyParser(argv: Array[String]) extends TaggerTask[String](argv) {

  import NonFactoredSerializer._

  lazy val trainer = new GenericNonFactoredTrainer[String](true,opts) with MstCrfTraining[String] {
    // should get rid of this for dependency parsing - number of states varies by sentence 
    // this just acts as a global upperbound at the moment - so pick a high value like 100
    val nstates = opts.numStates.getOrElse(100) 
    val mgr = new DynamicDepParserFeatureManager(opts.nonFactoredFeatureSpec.get,nstates)
    val fr = new NonFactoredFeatureRep[String](mgr,false,opts.numStates.getOrElse(0),true)
    val sGen = new NonFactoredTrainingSeqGen[String] (fr, opts) with XmlParserSeqGen 
  }
  
  lazy val decoder = new NonFactoredDecoder[String](true,opts) {
    val model = readModel(opts.model.get)
    println("loaded in model: " + opts.model.get)
    opts.modelDump match {case Some(mf) => model.print(new java.io.File(mf)) case None => }
    override lazy val viterbiDecoder = new MstMAPInference(model.crf)
    val mgr = new DynamicDepParserFeatureManager(model.fspec,model.numStates)
    val fr = new DecodingNonFactoredFeatureRep[String](model.faMap,mgr,model.numStates,true)
    //val fr = new NonFactoredFeatureRep[String](mgr, false, model.numStates, true)
    val sGen = new NonFactoredDecodingSeqGen[String] (fr, model, opts) with XmlParserSeqGen
  }	    
}

object ParserTask {
  def printUsage = println(" Usage: ")
  
  def main(argv: Array[String]) : Unit = {
    val tagger = new DependencyParser(argv) 
    tagger.process()
  }
}
