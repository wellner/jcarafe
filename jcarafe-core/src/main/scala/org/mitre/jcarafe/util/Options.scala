/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */
package org.mitre.jcarafe.util

/*
 * This will handle options provided to the trainer and decoder applications
*/

abstract class CommandLineHandler {
  val regStrMap = new scala.collection.mutable.HashMap[String,String]
  val regFlagMap = new scala.collection.mutable.HashMap[String,String]
  val regMultiMap = new scala.collection.mutable.HashMap[String,String]
  
  class ToParam(val n: String) {
    def desc(y: String) = regStrMap += (n -> y)
    def multi(y: String) = regMultiMap += (n -> y)
    def flag(y: String) = regFlagMap += (n -> y)
  }
  implicit def conv(n: String) = { new ToParam(n) }
  
  def printRegistered = {
    val all = (regStrMap ++ regFlagMap ++ regMultiMap).toSeq.sortWith(_._1 < _._1)
    all foreach {kv => kv match {case (k,v) => println("%30s\t\t%s".format(k,v))} }
  }
  
  val stringOpts = new scala.collection.mutable.HashMap[String,String]
  val multiOpts = new scala.collection.mutable.HashMap[String,List[String]]
  val flagOpts = new scala.collection.mutable.HashSet[String]
  def commandExit : Unit = {
    printRegistered
    sys.exit(2)
  }
  
  def _process(in: List[String]) : Unit = in match {
    case x :: xs if (!x.startsWith("-") || x == "--help") => commandExit
    case x :: xs if regFlagMap.contains(x) => flagOpts += x; _process(xs)
    case x :: y :: xs if regMultiMap.contains(x) =>
      val cur = multiOpts.get(x) match {case Some(c) => c case None => Nil} 
      multiOpts += (x -> (y :: cur))
      _process(xs) 
    case x :: y :: xs if regStrMap.contains(x) =>
      stringOpts += (x -> y)
      _process(xs)
    case Nil => 
    case x :: _ => 
      println("Unrecognized option: " + x)
      commandExit 
  }
  
  def process(in: List[String]) : Unit = { 
    _process(in)
    // check for required arguments
  }

  def get(s: String) = stringOpts.get(s)
  def check(s: String) = flagOpts.contains(s)
  def getAll(s: String) = multiOpts.get(s) match {case Some(x) => x case None => Nil}
}

class BaseOptionHandler(params: Array[String], val check: Boolean) extends CommandLineHandler {
  def this() = this(Array(),false)
	
  def process() : Unit = {
    process(params.toList) 
    /*
    else if (params.length == 0) {} 
    else {
      println("Invalid command line parameters: ")
      params foreach { println } 
      commandExit
    }
    */
    if (check && !(multiOpts.contains("--model"))) {
	  	   println("--model argument required")
	  	   commandExit
	   }
  }
}


class OptionHandler(params: Array[String], check: Boolean) extends BaseOptionHandler(params,check) {
  def this(params: Array[String]) = this(params,false)

  "--mode"           desc "Input/output mode: inline|json|basic"
  "--train"          flag "Perform training/estimation; rather than decoding (the default)"
  "--no-pre-proc"    flag "Do NOT Tokenize input"
  "--input-dir"      desc "Input directory"
  "--filter"         desc "Regular expression filter for input directory files"
  "--model"          desc "Model file destination"
  "--input-file"     desc "Input file"
  "--tagset"         desc "Tagset specification file"
  "--tag"            multi "Specific simple tag/label"
  "--seed"           desc "Seed random sampling"
  "--strip"          flag "Strip original tags if present when outputting decoded files"
  "--folds"          desc "Number of folds"  
  //"--recode" multi "Recode sequences"
  "--num-states"     desc "Number of states for non-factored model"
  "--seq-boundary"   multi "Sequence boundary label/tag"
  "--region"         multi "Region(s) to consider for processing (when in JSON mode)"
  "--pre-model"      multi "Models to use for pre-processing"  
  "--disk-cache"     desc  "Directory to place disk cache in"

  "--lexicon-dir"        desc "Lexicon directory"
  "--word-properties"    desc "File with word properties/features"
  "--word-scores"        desc "File with word frequency scores"
  "--max-iters"          desc "Maximum number of training iterations"
  "--no-begin"           flag "Do NOT use begin encoding of states"
  "--gaussian-prior"     desc "Gaussian prior regularizer"
  "--semi-crf"           flag "Use Semi-CRF model"
  "--psa"                flag "Use Stochastic Gradient Descent training with PSA updates"
  "--sgd"                flag "Use Stochastic Gradient Descent training"
  "--momentum"           desc "Use momentum term in Stochastic Gradient descent (default 0.0) "
  "--l1"                 flag "Use L1 regularization (only available with --psa or --sgd options)" 
  "--l1-C"               desc "C parameter for use with L1 regularization"
  "--learning-rate"      desc "Initial learning rate to use for PSA training" 
  "--hidden-learning-rate" desc "Initial learning rate to use for PSA/SGD training for hidden layer input weights (when using Neural CRF)" 
  "--batch-size"         desc "Batch size for stochastic gradient descent training"
  "--parallel"           flag "Parallelize feature expectation computation"
  "--fspec"              desc "Feature specification file"
  "--non-factored-fspec" desc "Feature specification file for non-factored representations"

  "--period-size"        desc "Period size for updating learning rates using PSA"
  "--mdump"              desc "Model dump file"
  "--cpus"               desc "Number of CPUs to use in parallel training"
  "--neural"             flag "Use a hidden neuron layer"
  "--num-gates"          desc "Number of hidden nodes/gates"
  "--no-cache"           flag "Do NOT cache expanded feature vectors"
  "--feature-split-with-space" flag "Use spaces rather than tabs to delimit features in BASIC mode"

  "--keep-tokens"        flag "Keep tokens (i.e. lex tags) in output"
  "--output-file"        desc "Output file location"
  "--output-dir"         desc "Output directory location"
  "--out-suffix"         desc "Output suffix"
  "--prior-adjust"       desc "Bias/adjust state prior weights in model"
  "--prior-weights"      desc "File containing per-label weights to bias decoder output"
  "--evaluate"           desc "Evaluate decoder on gold-standard test data"
  "--no-tags"            flag "Indicates that the trainer/decoder should not try to parse SGML/XML elements (i.e. tags)"
  "--streaming"          flag "Process files in a streaming fashion; faster and requires less memory for larger files"
  "--nthreads"           desc "Number of threads to use for feature extraction during decoding"
  "--weighted-feature-map" desc "Induce weighted feature map from auxillary data"
  "--ss-iters"           desc "Number of iterations for self-induced feature parameterization"
  "--unlabeled-input-dir" desc "Directory containing files of unlabeled data for use with semi-supervised learning"
  "--weighted-feature-vectors" desc "Induced feature vectors"
  "--confidences"        flag "Output sequence and token posterior probabilities (JSON mode only)"
  "--posteriors"         flag "Output sequence and token posterior distributions (JSON mode only)"
  "--p-alpha"            desc "Set the p-alpha for SGD training"
  "--no-sent-break"      flag "Do NOT break sentences/sequences - treat entire zones/regions as single sequences"
  "--tokenizer-patterns" desc "Split and Merge tokenizer post-processing patterns"
  "--each-iteration"     flag "Dump model file out on each training iteration"
  "--num-random-features" desc "Size of Random Feature Vector"
  "--num-semirandom-supported-features" desc "Size of semi-random supported feature vector"
  "--random-features"    flag "Use unsupported features; randomized"
  "--random-feature-coefficient" desc "Multiplicative factor for number of random features (default = 3.0)"
  "--granularity"        desc "Size of sequence batches for large files (default 5000)"
  "--partially-labeled"  flag "Enable training over partially labeled sequences"
  "--uncertain-tag"      desc "Tag denoting regions of unknown/uncertain tags"
  "--raw-cache"          flag "Applicable with the disk-cache option; caches raw source data (recomputes features repeatedly)"
  "--empirical-dist-train" flag "Train using empirical per-state marginal distributions"
}

class Options(val argv: Array[String], val optHandler: BaseOptionHandler, val processArgs: Boolean = true) {
  def this(argv: Array[String]) = this(argv,new OptionHandler(argv, false), true) 
  def this() = this(Array()) 

  import Tagset.parseTagSpec
  import Tagset.loadTagset

  val utf8Codec = scala.io.Codec("utf-8")

  if (processArgs) optHandler.process // process the arguments

  var required : List[String] = Nil

  def addToRequired(s: String) = {required = s :: required}

  def checkRequired() : Unit = {
    val ok = required forall {r => (optHandler.check(r) || (optHandler.get(r) != None) || (optHandler.getAll(r).size > 0)) }
    if (!ok) {
      System.err.println("\nRequired arguments missing.")
      System.err.println("Expected: ")
      System.err.println(required.mkString("\n"))
      sys.exit(2)
    } 
  }

  def checkRequired(ss: String*) : Unit = {
    required = ss.toList
    checkRequired()
  }

  /*
   * Automatically convert Option[String] to Option[Int] as needed 
  */
  implicit def toIntOption(s: Option[String]) : Option[Int] = s match {case Some(n) => Some(n.toInt) case None => None}

  var keepToks = optHandler.check("--keep-tokens")
  var train = optHandler.check("--train")
  var mode = optHandler.get("--mode")
  var neural = optHandler.check("--neural")
  var numGates = optHandler.get("--num-gates") match {case Some(v) => v.toInt case None => 0}

  var zoneset : Tagset =
    optHandler.get("--regionset") match {
      case Some(t) => loadTagset(t) 
      case None => 
	val regions = optHandler.getAll("--region")
	new Tagset(regions.foldLeft(Set.empty:Set[AbstractLabel]){(ac,s) => ac + parseTagSpec(s)})}
  
  var tagset : Tagset = 
    optHandler.get("--tagset") match {
      case Some(t) => loadTagset(t) 
      case None => 
        new Tagset(optHandler.getAll("--tag").foldLeft(Set.empty:Set[AbstractLabel]){(ac,s) => ac + parseTagSpec(s)})}
  
  var uncertainTag = optHandler.get("--uncertain-tag")
  
  var stripOriginalTags = optHandler.check("--strip")

  var boundaries : Tagset = 
    new Tagset(optHandler.getAll("--seq-boundary").foldLeft(Set.empty:Set[AbstractLabel]){(ac,s) => ac + parseTagSpec(s)})

  var semiCrf : Boolean               = optHandler.check("--semi-crf") 
  var inputDir : Option[String]       = optHandler.get("--input-dir")
  var inputFile : Option[String]      = optHandler.get("--input-file")
  var inputFilter : Option[String]    = optHandler.get("--filter")
  var model : Option[String]          = optHandler.get("--model") 

  var lexDir : Option[String]         = optHandler.get("--lexicon-dir")
  var wordPropFile : Option[String]   = optHandler.get("--word-properties")
  var wordScoreFile : Option[String]   = optHandler.get("--word-scores")
  var preProc : Boolean               = !(optHandler.check("--no-pre-proc"))
  var numStates: Option[Int]          = optHandler.get("--num-states")
  var noCache : Boolean               = optHandler.check("--no-cache")

  // NOTE: at this stage the models will be applied in the order in which they appear on the command line!!
  var preModels                       = optHandler.getAll("--pre-model").reverse // the models for decoding

  var diskCache                       = optHandler.get("--disk-cache")

  var initialModel                    = optHandler.get("--initial-model")
  var priorAdjust                     = optHandler.get("--prior-adjust") match {case Some(v) => Some(v.toDouble) case None => None}
  var priorWeightFile                 = optHandler.get("--prior-weights")
  var outputFile                      = optHandler.get("--output-file")
  var outputDir                       = optHandler.get("--output-dir")
  var outSuffix                       = optHandler.get("--out-suffix")
  var evaluate                        = optHandler.get("--evaluate") 
  var rawDecode                       = optHandler.check("--no-tags") 
  var streaming                       = optHandler.check("--streaming") 
  var numThreads : Option[Int]        = optHandler.get("--nthreads") 
  var useSpaces : Boolean             = optHandler.check("--feature-split-with-space")

  var psa                             = optHandler.check("--psa") 
  var noBegin                         = optHandler.check("--no-begin") || optHandler.check("--semi-crf")
  var gaussian                        = optHandler.get("--gaussian-prior") match {case Some(v) => v.toDouble case None => 10.0}
  var learningRate                    = optHandler.get("--learning-rate") match {case Some(v) => v.toDouble case None => 0.1}
  var hiddenLearningRate              = optHandler.get("--hidden-learning-rate") 
  var psaPeriodSize                   = optHandler.get("--period-size") match {case Some(v) => v.toInt case None => 10}
  var parallel                        = optHandler.check("--parallel")
  var numCPUs : Option[Int]           = optHandler.get("--cpus") 
  var featureSpec : Option[String]    = optHandler.get("--fspec")
  var nonFactoredFeatureSpec          = optHandler.get("--non-factored-fspec") 
  var modelDump: Option[String]       = optHandler.get("--mdump")
  var l1                              = optHandler.check("--l1")
  var sgd                             = optHandler.check("--sgd")
  var momentum                        = optHandler.get("--momentum") match {case Some(v) => v.toDouble case None => 0.0}
  var maxIters : Int                  = optHandler.get("--max-iters") match {case Some(v) => v.toInt case None => if (psa || sgd) 10 else 200 }
  var seed : Option[Int]              = optHandler.get("--seed")
  var batchSize : Int                 = optHandler.get("--batch-size") match {case Some(v) => v.toInt case None => 1} 
  var CValue : Double                 = optHandler.get("--l1-C") match {case Some(v) => v.toDouble case None => 0.1} 
  var empDistTrain : Boolean          = optHandler.check("--empirical-dist-train")
  var weightedFeatureMap              = optHandler.get("--weighted-feature-map")
  var selfInducedIterations           = optHandler.get("--ss-iters") match {case Some(v) => v.toInt case None => 0} 
  var unlabeledInputDir               = optHandler.get("--unlabeled-input-dir")

  var xValFolds                       = optHandler.get("--folds") map {x => x.toInt}
  var report                          = optHandler.get("--report")
  var inducedFVecsFile                = optHandler.get("--weighted-feature-vectors")
  var posteriors                      = optHandler.check("--posteriors")
  var confidences                     = optHandler.check("--confidences")
  var pAlpha : Double                 = optHandler.get("--p-alpha") match {case Some(v) => v.toDouble case None => 0.9}
  var noSentBreak                     = optHandler.check("--no-sent-break")
  var tokenizerPatterns               = optHandler.get("--tokenizer-patterns")
  var eachIteration                   = optHandler.check("--each-iteration")
  var numRandomFeatures               = optHandler.get("--num-random-features") match {case Some(v) => v.toInt case None => -1}
  var randomFeatures                  = optHandler.check("--random-features")
  var randomSupportedFeatures         = optHandler.check("--random-supported")
  var randomFeatureCoefficient        = optHandler.get("--random-feature-coefficient") match {case Some(v) => v.toDouble case None => 3.0}
  var granularity : Int               = optHandler.get("--granularity") match {case Some(v) => v.toInt case None => 1000}
  var partialLabels: Boolean          = optHandler.check("--partially-labeled")
  var rawCache: Boolean               = optHandler.check("--raw-cache")
  
  def setInto(no: Options) = {
    no.CValue_=(CValue)
    no.diskCache_=(diskCache)
    no.evaluate_=(evaluate)
    no.featureSpec_=(featureSpec)
    no.gaussian_=(gaussian)
    no.hiddenLearningRate_=(hiddenLearningRate)
    no.inducedFVecsFile_=(inducedFVecsFile)
    no.initialModel_=(initialModel)
    no.inputDir_=(inputDir)
    no.inputFile_=(inputFile)
    no.inputFilter_=(inputFilter)
    no.keepToks_=(keepToks)
    no.l1_=(l1)
    no.learningRate_=(learningRate)
    no.lexDir_=(lexDir)
    no.maxIters_=(maxIters)
    no.mode_=(mode)
    no.model_=(model)
    no.modelDump_=(modelDump)
    no.momentum_=(momentum)
    no.neural_=(neural)
    no.noBegin_=(noBegin)
    no.noCache_=(noCache)
    no.nonFactoredFeatureSpec_=(nonFactoredFeatureSpec)
    no.noSentBreak_=(noSentBreak)
    no.numCPUs_=(numCPUs)
    no.numGates_=(numGates)
    no.numStates_=(numStates)
    no.numThreads_=(numThreads)
    no.outputDir_=(outputDir)
    no.outputFile_=(outputFile)
    no.outSuffix_=(outSuffix)
    no.pAlpha_=(pAlpha)
    no.parallel_=(parallel)
    no.posteriors_=(posteriors)
    no.confidences_=(confidences)
    no.preModels_=(preModels)
    no.preProc_=(preProc)
    no.priorAdjust_=(priorAdjust)
    no.priorWeightFile_=(priorWeightFile)
    no.psa_=(psa)
    no.psaPeriodSize_=(psaPeriodSize)
    no.rawDecode_=(rawDecode)
    no.report_=(report)
    no.required_=(required)
    no.seed_=(seed)
    no.selfInducedIterations_=(selfInducedIterations)
    no.semiCrf_=(semiCrf)
    no.sgd_=(sgd)
    no.streaming_=(streaming)
    no.tagset_=(tagset)
    no.stripOriginalTags_=(stripOriginalTags)
    no.tokenizerPatterns_=(tokenizerPatterns)
    no.train_=(train)
    no.seed_=(seed)
    no.unlabeledInputDir_=(unlabeledInputDir)
    no.useSpaces_=(useSpaces)
    no.weightedFeatureMap_=(weightedFeatureMap)
    no.wordPropFile_=(wordPropFile)
    no.wordScoreFile_=(wordScoreFile)
    no.xValFolds_=(xValFolds)
    no.zoneset_=(zoneset)
    no.eachIteration_=(eachIteration)
    no.numRandomFeatures_=(numRandomFeatures)
    no.randomFeatureCoefficient_=(randomFeatureCoefficient)
    no.granularity_=(granularity)
    no.partialLabels_=(partialLabels)
    no.uncertainTag_=(uncertainTag)
    no.rawCache_=(rawCache)
    no.empDistTrain_=(empDistTrain)
  }
  
  def copy() : Options = {
    val no = new Options(this.argv, this.optHandler, false)
    setInto(no)
    no
  }
}

