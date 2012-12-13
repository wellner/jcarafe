package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{Trainer,InstanceSequence,CondLogLikelihoodLearner,NonFactoredSerializer,
  GenericNonFactoredTrainer, NonFactoredFeatureRep, NonFactoredTrainingSeqGen, NonFactoredDecoder,
  DecodingNonFactoredFeatureRep,NonFactoredDecodingSeqGen, PsaLearner, SeqGenScorer, WordProperties,CoreModel,LongAlphabet,NonFactoredModel,AbstractInstance
}
import org.mitre.jcarafe.tagger.TaggerTask

trait ProjectiveMstCrfTraining[O] extends Trainer[O] {
  
  val modelIterate: Option[(CoreModel,Int) => Unit] = None // override this to provide means to generate models on each training iteration
  
  def trainingRoutine(seqs: Seq[InstanceSequence]) = {
    println("Processed " + seqs.length + " sentences . . . beginning parameter estimation..\n")
    println("Number of features = " + sGen.getNumberOfFeatures)
    val mstCrf =
      if (opts.psa)
        new StochasticProjectiveMstCrf(sGen.getNumberOfFeatures, opts) with PsaLearner[AbstractInstance]
      else if (opts.parallel) 
        new ProjectiveMstCrfParallel(opts.numThreads.get, sGen.getNumberOfFeatures, opts.gaussian)
      else
    	new ProjectiveMstCrf(sGen.getNumberOfFeatures, opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
    trainModel(mstCrf,seqs,modelIterate)
  }
}

class ProjectiveDependencyParser(argv: Array[String]) extends TaggerTask[String](argv) {

  import NonFactoredSerializer._

  lazy val trainer = new GenericNonFactoredTrainer[String](true,opts) with ProjectiveMstCrfTraining[String] {
    // should get rid of this for dependency parsing - number of states varies by sentence 
    // this just acts as a global upperbound at the moment - so pick a high value like 1000
    val nstates = opts.numStates.getOrElse(1000) 
    val mgr = new DynamicDepParserFeatureManager(opts.nonFactoredFeatureSpec.get,nstates)
    mgr.wdProps match {
      case None => opts.wordPropFile match {case Some(f) => mgr.wdProps_=(Some(new WordProperties(f))) case None => } case Some(_) => }
    val fr = new NonFactoredFeatureRep[String](opts,mgr,false,opts.numStates.getOrElse(0),true)
    //val sGen = new NonFactoredTrainingSeqGen[String] (fr, opts) with XmlParserSeqGen
    val sGen = new NonFactoredTrainingSeqGen[String] (fr, opts) with CoNLLSeqGen
    override val modelIterate : Option[(CoreModel,Int) => Unit] = 
      Some({(cm,i) =>
        val fm : LongAlphabet = sGen.frep.faMap
        val m = new NonFactoredModel(sGen.getModelName, sGen.getLexicon, sGen.getWordProps, 1,cm,sGen.frep.faMap, opts.numStates.get)
        writeModel(m,new java.io.File(opts.model.get+"_"+i))
        })
    def xValidate() : Unit = throw new RuntimeException("X-validation not yet supported with Projective Dependency Parser")
  }
  
  lazy val decoder = new NonFactoredDecoder[String](true,opts) {
    val model = readModel(opts.model.get)
    model.fixAlphabet(true)
    opts.modelDump match {case Some(mf) => model.print(new java.io.File(mf)) case None => }
    override lazy val viterbiDecoder = new ProjectiveMstInference(model.crf)
    val mgr = new DynamicDepParserFeatureManager(model.fspec,model.numStates)
    mgr.wdProps match {
      case None => mgr.wdProps_=(model.wp) case Some(_) =>
    }
    val fr = new DecodingNonFactoredFeatureRep[String](model.faMap,mgr,model.numStates,true)
    val sGen = new NonFactoredDecodingSeqGen[String] (fr, model, opts) with CoNLLSeqGen
  }	    
}

object ProjectiveParserTask {
  
  def main(argv: Array[String]) : Unit = {
    val tagger = new ProjectiveDependencyParser(argv) 
    tagger.process()
  }
  
}