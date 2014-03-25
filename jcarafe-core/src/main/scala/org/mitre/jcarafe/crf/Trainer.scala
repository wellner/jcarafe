
/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import org.mitre.jcarafe.util._
import collection.mutable.ArrayBuffer

abstract class Trainer[Obs](val adjust: Boolean, val opts: Options) {
  def this(o: Options) = this(false, o)

  type TrSeqGen <: SeqGen[Obs]

  /**
   * A <code>SeqGen</code> object specialized for training
   */
  val sGen: TrSeqGen

  def trainModel(dCrf: Trainable[AbstractInstance], seqs: collection.immutable.IndexedSeq[InstanceSequence], modelIterFn: Option[(CoreModel, Int) => Unit] = None): Unit

  def trainingRoutine(seqs: collection.immutable.IndexedSeq[InstanceSequence]): Unit

  def trainFromSeqs(seqs: collection.immutable.IndexedSeq[SourceSequence[Obs]]) = {
    val aSeqs = sGen.extractFeatures(seqs)
    trainingRoutine(aSeqs)
  }

  /**
   * Train and return a model.
   * XX - use default parameters here
   */
  def train(): Unit = {
    val seqs: collection.immutable.IndexedSeq[InstanceSequence] = sGen.createSeqsFromFiles // this has to happen before generating CRF  
    trainingRoutine(seqs)
  }

  def xValidate(): Unit
  def xValidateFromSeqs(seqs: collection.immutable.IndexedSeq[SourceSequence[Obs]]) : Unit
}

trait LinearCRFTraining[Obs] extends Trainer[Obs] with SeqXValidator {
  
  def getNumPs = opts.numThreads match {
        case None => Runtime.getRuntime.availableProcessors * 4 / 5 // leave a CPU or two free
        case Some(n) => n
      }

  def getCrf(empDist: Boolean = false): Crf = {
    if (opts.neural) {
      NeuralCrf(sGen, opts)
    } else if (opts.semiCrf) {
      val s = sGen.getMaxSegmentSize
      CrfInstance.maxSegSize = s // "global" value for maximum seg size
      if (opts.psa)
        if (opts.l1)
          new StochasticSemiCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, (s + 1), opts) with PsaLearnerWithL1[AbstractInstance]
        else
          new StochasticSemiCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, (s + 1), opts) with PsaLearner[AbstractInstance]
      else
        new DenseSemiCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, (s + 1), opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
    } else if (opts.partialLabels) {
      println(">> Training with partially labeled sequences ... <<\n")
      if (opts.psa) {
        new StochasticGeneralizedEMCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with PsaLearner[AbstractInstance]
      } else {
        if (opts.parallel) {
          val numPs = getNumPs
          new DenseParallelGeneralizedEMCrf(numPs,sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with CondLogLikelihoodLearner[AbstractInstance]
        } else
          new DenseGeneralizedEMCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with CondLogLikelihoodLearner[AbstractInstance]
      }
    } else if (empDist) {
      println(">> Training with soft-labeled sequences ... using KL-divergence likelihood maximization <<\n")
      if (opts.psa) {
        new KLDivMinimizingStochasticCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with PsaLearner[AbstractInstance]
      }
      else {
        new KLDivMinimizingCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
      }
    } else if (opts.psa)
      if (opts.l1)
        new StochasticCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with PsaLearnerWithL1[AbstractInstance]
      else
        new StochasticCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with PsaLearner[AbstractInstance]
    else if (opts.parallel) {
      val numPs = getNumPs
      println(">> Initiating Parallel Training using " + numPs + " processors <<\n")
      new DenseParallelCrf(numPs, sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts.gaussian)
    } else if (opts.sgd) {
      if (opts.l1)
        new StochasticCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with SgdLearnerWithL1[AbstractInstance]
      else
        new StochasticCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts) with SgdLearner[AbstractInstance]
    } else {
      if (opts.semiCrf) {
        val s = sGen.getMaxSegmentSize
        CrfInstance.maxSegSize = s // "global" value for maximum seg size
        new DenseSemiCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, (s + 1), opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
      } else new DenseCrf(sGen.getNumberOfStates, sGen.getNumberOfFeatures, 1, opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
    }
  }

  def xValidate(): Unit = {
    val seqs: collection.immutable.IndexedSeq[InstanceSequence] = sGen.createSeqsFromFiles
    val crf = getCrf()
    if (adjust) crf.adjustible_=(true)
    val nf = opts.xValFolds.getOrElse(10)
    xValidate(sGen,crf,new MemoryAccessSeq(seqs, opts.seed), opts.maxIters, nf)
  }
  
  def xValidateFromSeqs(seqs: collection.immutable.IndexedSeq[SourceSequence[Obs]]) = {
    val aSeqs = sGen.extractFeatures(seqs)
    val crf = getCrf()
    if (adjust) crf.adjustible_=(true)
    val nf = opts.xValFolds.getOrElse(10)
    xValidate(sGen,crf,new MemoryAccessSeq(aSeqs, opts.seed), opts.maxIters, nf)
  }
  
  
  
  def printHeader(seqs: Seq[InstanceSequence]) : Unit = {
    println("Processed " + seqs.length + " sequences . . . beginning parameter estimation..\n")
    println("Number of features = " + (if (adjust) "DYNAMIC" else sGen.getNumberOfFeatures))
    println("Number of states   = " + sGen.getNumberOfStates)
    if (opts.randomFeatures || opts.randomSupportedFeatures)
      println("Number of feature types = " + sGen.frep.getNumberOfFeatureTypes)
    if (sGen.getMaxSegmentSize > 0)
      println("SEMI-CRF model used.   Maximum segment size = " + sGen.getMaxSegmentSize)
    if (opts.neural) println("Using hidden gates ... \"neural\" CRF ")
    
  }
  
  def trainingRoutine(seqs: collection.immutable.IndexedSeq[InstanceSequence]) = {    
    val dCrf: Crf = getCrf(opts.empDistTrain)
    if (adjust) dCrf.adjustible_=(true)
    val aseqs = if (opts.partialLabels && false) {
      val filteredSeqs = new ArrayBuffer[InstanceSequence]
      var numRemoved = 0
      seqs foreach {is => if (is.iseq.exists(v => v.label >= 0)) filteredSeqs append is else numRemoved += 1}
      println("\n ... Number of training sequences dropped due to lack of any provided labels ...")
      filteredSeqs.toIndexedSeq
    } else seqs
    printHeader(aseqs)
    trainModel(dCrf, aseqs)
  }

}

abstract class FactoredTrainer[O](opts: Options) extends Trainer[O](opts) with LinearCRFTraining[O] {

  import StandardSerializer._ // writing and reading models
  type TrSeqGen = TrainingSeqGen[O]

  def getStdModel(ss: Int, coreModel: CoreModel) = {
    val stM = new StdModel(sGen.getModelName, !opts.noBegin, sGen.getLexicon, sGen.getWordProps, sGen.getWordScores, sGen.getInducedFeatureMap, ss, sGen.getLAlphabet, coreModel, sGen.frep.fsetMap)
    if (opts.l1) Model.compactModel(stM) else stM
  }
  
  def getRandModel(ss: Int, coreModel: CoreModel) = {
    new RandomStdModel(sGen.getModelName, !opts.noBegin, sGen.getLexicon, sGen.getWordProps, 
        sGen.getWordScores, sGen.getInducedFeatureMap, ss, sGen.getLAlphabet, coreModel, 
        sGen.frep.faMap.asInstanceOf[RandomLongAlphabet])
  }

  def trainModel(dCrf: Trainable[AbstractInstance], seqs: collection.immutable.IndexedSeq[InstanceSequence], modelIterFn: Option[(CoreModel, Int) => Unit] = None) = {
    val accessSeq = new MemoryAccessSeq(seqs, opts.seed)
    val coreModel = dCrf.train(accessSeq, opts.maxIters, modelIterFn)
    val decoder = Viterbi(false,1,coreModel,true)
    if (opts.randomFeatures || opts.randomSupportedFeatures) {
      val m = getRandModel((sGen.getMaxSegmentSize + 1), coreModel)
      writeModel(m, new java.io.File(opts.model.get))
    } else {
      val m = getStdModel((sGen.getMaxSegmentSize + 1), coreModel)    
      writeModel(m, new java.io.File(opts.model.get))
    }
  }
}

abstract class GenericNonFactoredTrainer[O](adj: Boolean, opts: Options) extends Trainer[O](adj, opts) {

  import NonFactoredSerializer._
  type TrSeqGen = NonFactoredTrainingSeqGen[O]

  val supported = true // only use supported features

  def gatherFeatures(sourcePairSeqs: sGen.Seqs): Unit = {
    sourcePairSeqs map { dseq =>
      var sid = -1
      val ln = dseq.length
      var i = 0; while (i < ln) {
        sGen.frep.gatherFeatures(dseq, i) // just gather by adding to symbol table
        i += 1
      }
    }
  }

  def gatherFeatureSymbolsFromFiles: Unit = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new java.io.File(dirStr)
        dir.listFiles.toSeq filter
          { f: java.io.File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } } foreach
          { f: java.io.File => if (f.isFile) gatherFeatures(sGen.toSources(f)) }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            gatherFeatures(sGen.toSources(f))
          case None =>
            throw new RuntimeException("Expecting input file")
        }
    }
  }

  override def train() = {
    if (!supported) super.train()
    else {
      if (opts.numRandomFeatures < 0) gatherFeatureSymbolsFromFiles // gather supported features if not using Random Features
      //println("Symbol table created with " + sGen.frep.faMap.size + " feature symbols")
      sGen.frep.faMap.fixed_=(true) // fix the feature alphabet after now collecting just supported features
      super.train() // re-collect features and train model
    }
  }
  
  def xValidate() : Unit
  
  def trainModel(dCrf: Trainable[AbstractInstance], seqs: collection.immutable.IndexedSeq[InstanceSequence], modelIterFn: Option[(CoreModel, Int) => Unit] = None) = {
    val accessSeq = new MemoryAccessSeq(seqs, opts.seed)
    val coreModel = dCrf.train(accessSeq, opts.maxIters, modelIterFn)
    val fm: LongAlphabet = sGen.frep.faMap
    // XXX - need to yet write a separate model compactor for nonfactored models
    val m = new NonFactoredModel(sGen.getModelName, sGen.getLexicon, sGen.getWordProps, 1, coreModel, sGen.frep.faMap, opts.numStates.get)
    opts.modelDump match { case Some(md) => m.print(new java.io.File(md)) case None => }
    writeModel(m, new java.io.File(opts.model.get))
  }
}

abstract class NonFactoredTrainer[O](adj: Boolean, opts: Options) extends GenericNonFactoredTrainer[O](adj, opts)
  with LinearCRFTraining[O] {
  def this(opts: Options) = this(false, opts)
}

abstract class StdTrainer(opts: Options) extends FactoredTrainer[String](opts) {

}
