/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.collection.mutable.ListBuffer
import java.io.File
import org.mitre.jcarafe.util._

abstract class InstanceSequence(val st: Int, val en: Int) {
  var seqPosteriorProbability = 0.0
  def iseq: Seq[AbstractInstance]
  def length: Int
  def print() =
    iseq foreach { ai =>
      println("l => " + ai.label + " vecs: ")
      ai.getCompVec foreach { v =>
        v foreach { x => println("fid=> " + x.fid + " v = " + x.value) }
      }
    }
}

class MemoryInstanceSequence(val internalSeq: Seq[AbstractInstance], st: Int, en: Int) extends InstanceSequence(st, en) {
  def this(iseq: Seq[AbstractInstance]) = this(iseq, -1, -1)
  def length = iseq.length
  def iseq = internalSeq
}

abstract class DiskInstanceSequence(val fp: java.io.File, st: Int, en: Int, val len: Int) extends InstanceSequence(st, en) {

  def iseq: Seq[AbstractInstance]
  def length: Int = len
}


class FactoredCachedSourceSequence[T](val sGen: TrainingSeqGen[T], val src: SourceSequence[T], st: Int, en: Int) extends InstanceSequence(st, en) {
  def iseq: Seq[AbstractInstance] = sGen.extractFeaturesDirect(src)
  override lazy val length = iseq.length
}

class NonFactoredCachedSourceSequence[T](val sGen: SeqGen[T], val src: SourceSequence[T], st: Int, en: Int) extends InstanceSequence(st, en) {

  def iseq: Seq[AbstractInstance] = sGen.extractFeatures(src).iseq
  def length: Int = iseq.length
}

object InstSeq {
  import InstanceSerializations._
  import com.twitter.chill.{ AllScalaRegistrar, EmptyScalaKryoInstantiator }
  import com.esotericsoftware.kryo.io.{ Output, Input }
  var icnt = 0

  val instantiator = new EmptyScalaKryoInstantiator
  val kryo = instantiator.newKryo
  (new AllScalaRegistrar).apply(kryo) // register all Scala types
  kryo.register(classOf[CrfInstance])
  kryo.register(classOf[SourceSequence[String]])
  kryo.register(classOf[NonFactoredCrfInstance])
  //kryo.register(classOf[Seq[NonFactoredCrfInstance]])
  //kryo.register(classOf[Seq[CrfInstance]])
  
  class NonFactoredCrfDiskInstanceSequence(fp: java.io.File, st: Int, en: Int, ln: Int) extends DiskInstanceSequence(fp, st, en, ln) {
    
    def iseq: Seq[AbstractInstance] = {
      val is = new java.io.BufferedInputStream(new java.io.FileInputStream(fp))
      val kInput = new Input(is)
      val o = kryo.readObject(kInput, classOf[Seq[NonFactoredCrfInstance]])
      is.close()
      kInput.close()
      o
    }
  }

  class RawInstanceSequenceStringObs(val sGen: TrainingSeqGen[String], fp: java.io.File, st: Int, en: Int, ln: Int) extends DiskInstanceSequence(fp, st, en, ln) {
    // serialize just the plain 
    def iseq: Seq[AbstractInstance] = {
      val kInput = new Input(new java.io.BufferedInputStream(new java.io.FileInputStream(fp)))    
      val src = kryo.readObject(kInput, classOf[SourceSequence[String]])
      kInput.close
      sGen.extractFeaturesDirect(src)
    }
    override lazy val length = iseq.length
  }

  class CrfDiskInstanceSequence(fp: java.io.File, st: Int, en: Int, ln: Int) extends DiskInstanceSequence(fp, st, en, ln) {
    
    def iseq: Seq[AbstractInstance] = {
      val is = new java.io.BufferedInputStream(new java.io.FileInputStream(fp))
      val kInput = new Input(is)
      val ss = kryo.readObject(kInput, classOf[Seq[CrfInstance]])
      kInput.close()
      ss
    }
  }
    

  def serializeSourceSeqToFile(ss: SourceSequence[String], f: java.io.File): Unit = {
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val kOutput = new Output(os)
    kryo.writeObject(kOutput, ss)
    kOutput.close
    os.close
  }
  
  def serializeNonFactoredToFile(ss: Seq[NonFactoredCrfInstance], f: java.io.File) = {
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val kOutput = new Output(os)
    kryo.writeObject(kOutput, ss)
    kOutput.close
    os.close
  }

  def serializeCrfInstanceToFile(ss: Seq[CrfInstance], f: java.io.File) = {
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val kOutput = new Output(os)
    kryo.writeObject(kOutput, ss)
    kOutput.close
    os.close
  }

  /*
   * This method creates a new InstanceSequence, RawInstanceSequenceStringObs that lazily re-computes the feature vector for the instance.
   * WARNING: This method only works for observation sequences parameterized with type java.lang.String since it 
   * must serialize the instance to disk with a specific type.
   */
  def apply[T](sg: TrainingSeqGen[T], ss: SourceSequence[T], st: Int, en: Int)(implicit m: Manifest[T]): InstanceSequence = {
    CrfInstance.diskCache match {
      case Some(filePath) =>
        val ofile = new java.io.File(filePath + "/" + icnt)
        icnt += 1
        if (m.toString equals "java.lang.String") { // should be a better way to set this up...
          val ssA = ss.asInstanceOf[SourceSequence[String]]
          val sgA = sg.asInstanceOf[TrainingSeqGen[String]]
          serializeSourceSeqToFile(ssA, ofile)
          new RawInstanceSequenceStringObs(sgA, ofile, st, en, ss.length)
        } else throw new RuntimeException("Disk caching only allowed with SourceSequence[T] where T = String")
      case None => new FactoredCachedSourceSequence(sg, ss, st, en)
    }
  }
  def apply(s: Seq[AbstractInstance], st: Int, en: Int): InstanceSequence = {
    CrfInstance.diskCache match {
      case Some(filePath) =>
        val ofile = new java.io.File(filePath + "/" + icnt)
        icnt += 1
        s match {
          case s: Seq[CrfInstance] =>
            serializeCrfInstanceToFile(s, ofile)
            new CrfDiskInstanceSequence(ofile, st, en, s.length)
            /*
          case s: Seq[NonFactoredCrfInstance] =>
            serializeNonFactoredToFile(s, ofile)
            new NonFactoredCrfDiskInstanceSequence(ofile, st, en, s.length)
            * 
            */
        }
      case None => new MemoryInstanceSequence(s, st, en)
    }
  }
  def apply(s: Seq[AbstractInstance]): InstanceSequence = apply(s, -1, -1)
  def apply[T](sg: SeqGen[T], s: SourceSequence[T]) = {
    new NonFactoredCachedSourceSequence(sg, s, -1, -1)
  }
}

class SourceSequence[Obs](val seq: Seq[ObsSource[Obs]], val parentSeq: Option[SourceSequence[Obs]], val st: Int, val en: Int) {
  def this(s: Seq[ObsSource[Obs]], par: Option[SourceSequence[Obs]]) = this(s, par, -1, -1)
  def this(s: Seq[ObsSource[Obs]]) = this(s, None)
  def this(s: Seq[ObsSource[Obs]], st: Int, en: Int) = this(s, None, st, en)
  def this(s: Seq[ObsSource[Obs]], par: SourceSequence[Obs]) = this(s, Some(par))

  def getInfo(k: String): Option[String] = None // subclasses can store and retrieve info here
  def apply(i: Int) = seq(i)
  def length = seq.length
  def updateLabels(nlabs: SourceSequence[Obs]) =
    (seq.toList zip nlabs.seq.toList) foreach { case (n, o) => n.label = o.label }

  def generateOneLongString = {
    val sb = new StringBuilder
    seq foreach { o => sb append o.obs.toString }
    sb.toString
  }
}

/**
 * Encapsulates functionality for creating labeled sequences.  This includes
 * extracting features over elements in a sequence.
 * @param opts        A set of command-line options passed in by user
 * @author Ben Wellner
 */
abstract class SeqGen[Obs](val opts: Options) {

  type DeserializationT <: Deserialization
  type Src = ObsSource[Obs]
  type Seqs = Seq[SourceSequence[Obs]]

  type FRepT <: FeatureRep[Obs]
  val frep: FRepT
  //val lAlphabet : Alphabet[AbstractLabel]
  val lAlphabet =
    if (opts.partialLabels) new AlphabetWithSpecialCases(false, { x: AbstractLabel => x.uncertain })
    else new Alphabet[AbstractLabel]
  val recodeAlphabet = new Alphabet[AbstractLabel]
  val unrecodeAlphabet = new Alphabet[AbstractLabel]

  val recode = false

  /*
   * This is lazy so that we can define it up front and only when invoked will it 
   * then create the inverse label alphabet
  */
  lazy val invLa: scala.collection.mutable.Map[Int, AbstractLabel] = lAlphabet.getInvMap

  var addBeginStates = false
  def otherIndex: Option[Int] = {
    val al = SLabel("lex")
    if (lAlphabet.contains(al)) {
      Some(lAlphabet(al))
    } else {
      None
    }
  }
  val boundaries: Tagset
  val printExistingTags = true

  def getMaxSegmentSize = frep.maxSegSize

  def getNumberOfFeatures: Int

  def getNumberOfNeuralFeatures: Int = 0

  def getState(l: AbstractLabel, b: Boolean) =
    l match {
      case SLabel("lex") => l
      case _ => if (b && !l.uncertain) BeginState(l) else l
    }

  /**
   * Creates a source sequence.  Subclasses can over-ride this to add additional infor regarding sequences
   * @param ss
   * @return SourceSequence[Obs] - observation sequence
   */
  def createSourceSequence(ss: Seq[ObsSource[String]]): SourceSequence[String] = new SourceSequence(ss)

  def createSourceSequence(ss: Seq[ObsSource[String]], st: Int, en: Int): SourceSequence[String] = new SourceSequence(ss, st, en)

  /**
   * Return the number of states in the model (when the size is fixed across the dataset)
   * @return Number of states in the model
   */
  def getNumberOfStates: Int = lAlphabet.size

  /**
   * Return the name of the feature manager instance (i.e. feature set name)
   * @return  A feature set name String
   */
  def getModelName: String = frep.getFeatureSetName

  /**
   * Return the lexicon used by the feature manager
   * @return An optional lexicon
   */
  def getLexicon: Option[BloomLexicon] = frep.getLexicon

  /**
   * Return the lexicon used by the feature manager
   * @return An optional lexicon
   */
  def getWordProps: Option[WordProperties] = frep.getWordProps

  /**
   * Return the lexicon used by the feature manager
   * @return An optional lexicon
   */
  def getWordScores: Option[WordScores] = frep.getWordScores

  /**
   * Return the lexicon used by the feature manager
   * @return An optional lexicon
   */
  def getInducedFeatureMap: Option[InducedFeatureMap] = frep.getInducedFeatureMap

  /**
   * Return the label Alphabet to use.  Should be overrided with a different alphabet
   * when doing recoding, for example.
   * @return    A label alphabet to pass to the trainer/decoder
   */
  def getLAlphabet = lAlphabet

  /**
   * Create a deserialization from a file path provided as a String
   * @param file   File path as a string
   * @return       A deserialized object
   */
  def deserializeFromFile(file: String): DeserializationT

  /**
   * Create a deserialization from a File object
   * @param file   File object
   * @return       A deserialized object
   */
  def deserializeFromFile(file: File): DeserializationT = deserializeFromFile(file.toString)

  /**
   * Create a deserialized object from a string containing a serialized representation
   * @param string  A string representing a serialized object
   * @return        A deserialized object
   */
  def deserializeFromString(string: String): DeserializationT

  /**
   * Create a deserialized object from a list of tokens
   * @param string  A a list of string tokens representing a serialized object
   * @return        A deserialized object
   */
  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT

  /**
   * Takes a file path (as string) and converts it to a sequence of sequences
   * of <code>ObsSource</code> objects
   * @param file    A file path as a string
   * @return        A sequence of sequences of <code>ObsSource</code> objects that represent observations
   *                and auxiliary information.
   */
  def toSources(file: String): Seqs = toSources(new java.io.File(file))
  def toSources(file: File): Seqs = toSources(deserializeFromFile(file))

  /**
   * Computes a sequence of sequences of <code>ObsSource</code> objects from a given deserialized object
   * @param deserialization   An input representation. For example an Xml DOM structure or a JSON structure
   *                          that represents input data and training exemplars as standoff annotations
   * @return       A sequence of sequences of <code>ObsSource</code> objects
   */
  def toSources(d: DeserializationT): Seqs

  def createSeqsWithInput(d: DeserializationT): Seq[InstanceSequence] = extractFeatures(toSources(d))

  def createSeqsWithInput(dseq: Seq[DeserializationT]): Seq[InstanceSequence] =
    dseq flatMap { (d: DeserializationT) => createSeqsWithInput(d) }

  private def gatherFiles: Seq[File] = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new File(dirStr)
        dir.listFiles.toSeq filter
          { f: File =>
            if (!f.isFile) false
            else pat.findFirstIn(f.toString) match { case Some(_) => true case None => false }
          }
      case None =>
        opts.inputFile match {
          case Some(f) => Seq(new File(f))
          case None =>
            throw new RuntimeException("Expecting input file")
        }
    }
  }

  def createSourcesFromFiles: Seq[Seqs] = gatherFiles map toSources

  def countFeatureTypesFromFiles: Unit = gatherFiles foreach { f => countFeatureTypes(toSources(f)) }

  def createInstancesFromFiles: Seq[InstanceSequence] = {
    if (opts.randomFeatures || opts.randomSupportedFeatures) countFeatureTypesFromFiles
    gatherFiles flatMap { f => extractFeatures(toSources(f)) }
  }

  //def createSeqsFromFiles : Seq[InstanceSequence] = extractFeaturesSeq(createSourcesFromFiles)
  def createSeqsFromFiles: Seq[InstanceSequence] = {
    if (opts.multiLine) {
      val sbuf = new collection.mutable.ListBuffer[InstanceSequence]
      var nread = 1
      val seqs = gatherFiles foreach { f =>
        val src = io.Source.fromFile(f)("UTF-8")
        val lines = src.getLines.toList
        lines foreach { l =>
          // extracting features immediately here will trigger disk-caching of source sequences as lines are read in, saving memory
          val instSeqs = extractFeatures(toSources(deserializeFromString(l)))
          nread += 1
          if ((nread % 1000) == 0) println("Read " + nread + " serialized training documents")
          sbuf ++= instSeqs
        }
      }
      sbuf.toSeq
    } else createInstancesFromFiles // this does each file separately which will be more efficient with disk caching
  }

  def extractFeatures(spSeqs: Seqs): Seq[InstanceSequence]

  def countFeatureTypes(src: SourceSequence[Obs]): Unit = {}

  def countFeatureTypes(spSeqs: Seqs): Unit = spSeqs.seq foreach countFeatureTypes

  def extractFeatures(src: SourceSequence[Obs]): InstanceSequence

  // extract features seq is present here so that extractFeatures can be applied separately to each "document"
  // so that document-specific global information related to "displaced features" can be retained and properly flushed
  def extractFeaturesSeq(sourcePairSeqsSeq: Seq[Seqs]): Seq[InstanceSequence] = sourcePairSeqsSeq flatMap extractFeatures

  // create a source - always have it be a beginning if the state corresponds to "otherIndex"
  protected def createSourceI(i: Int, o: Obs, b: Boolean, m: Option[Map[String, String]]) = frep.createSource(i, o, (b || (otherIndex match { case Some(oi) => oi == i case None => false })), m)
  protected def getIndex(l: AbstractLabel) = l match { case ILabel(i) => i case _ => lAlphabet.update(l) }

  def createSource(l: AbstractLabel, o: Obs, beg: Boolean) = createSourceI(getIndex(l), o, beg, None)

  def createSource(l: AbstractLabel, o: Obs) = createSourceI(getIndex(l), o, true, None)

  def createSource(l: AbstractLabel, o: Obs, beg: Boolean, i: Map[String, String]) = createSourceI(getIndex(l), o, beg, Some(i))
  def createSource(l: AbstractLabel, o: Obs, i: Map[String, String]) = createSourceI(getIndex(l), o, false, Some(i))
  def createSource(o: Obs, i: Map[String, String]) = createSourceI(-1, o, false, Some(i)) // label of -1 means that the label is missing/unknown

  def createDistributionalSource(dist: List[(AbstractLabel, Double)], obs: Obs, beg: Boolean, i: Map[String, String]): ObsSource[Obs] =
    frep.createDistributionalSource(dist map { case (al, s) => (getIndex(al), s) }, obs, beg, Some(i))

  // ---- Stuff for handling scoring/evaluation - possibly useful for Training time as well, so put this here rather than in DecodingSeqGen
  var totalTokCnt = 0
  var totalIncorrectTok = 0

  def reset() = {
    totalTokCnt = 0
    totalIncorrectTok = 0
  }

  def cleanUp(): Unit = {}

  def getAccuracy: Double = {
    ((totalTokCnt.toDouble - totalIncorrectTok.toDouble) / totalTokCnt.toDouble)
  }

  def evaluateSequences(seqs: Seq[InstanceSequence]) =
    seqs foreach { s =>
      for (i <- 0 until s.length) {
        val inst = s.iseq(i)
        totalTokCnt += 1
        if (!(inst.label == inst.orig)) totalIncorrectTok += 1
      }
    }

}

trait FactoredSeqGen[Obs] extends SeqGen[Obs] {
  val other: AbstractLabel = SLabel("lex")
  object StateCache {
    var beginStateCache: Option[scala.collection.mutable.HashSet[Int]] = None
    def updateStateCache[AbstractLabel](lAlphabet: Alphabet[AbstractLabel]) =
      beginStateCache match {
        case Some(s) =>
        case None =>
          synchronized {
            val c = new scala.collection.mutable.HashSet[Int]
            lAlphabet.mp.foreach { case (k, v) => k match { case BeginState(_) => c += v case _ => } }
            beginStateCache = Some(c)
          }
      }
    def isBegin(i: Int) = beginStateCache match { case Some(c) => c.contains(i) case None => false }
  }

}

abstract class TrainingSeqGen[Obs](fr: TrainingFactoredFeatureRep[Obs], opts: Options) extends SeqGen[Obs](opts) {

  type FRepT = TrainingFactoredFeatureRep[Obs]
  val frep: FRepT = fr

  def this(opts: Options) = this(new TrainingFactoredFeatureRep[Obs](opts), opts)

  def getNumberOfFeatures = frep.faMap.size
  override def getNumberOfNeuralFeatures = frep.neuralFaMap.size

  val boundaries = opts.boundaries
  this.addBeginStates_=(!opts.noBegin)

  private def extractSemiCrfFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] = {
    val insts = sourcePairSeqs map
      { dseq =>
        var sid = -1
        val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
          if (dseq(i).beg) sid += 1
          val inst = frep.createInstance(dseq(i), sid)
          frep.extractSupportedFeatures(inst, dseq, i)
          inst
        }
        iseq
      }
    // now actually extract features
    var i = 0
    insts map { instSeq =>
      val dseq = sourcePairSeqs(i)
      var pos = 0
      instSeq foreach { inst => frep.applyFeatureFns(inst, dseq, pos, true); pos += 1 }
      i += 1
      InstSeq(instSeq, dseq.st, dseq.en)
    }
  }

  def extractFeaturesDirect(dseq: SourceSequence[Obs]): Seq[AbstractInstance] = {
    var sid = -1
    val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
      if (dseq(i).beg) sid += 1
      val inst = frep.createInstance(dseq(i), sid)
      frep.applyFeatureFns(inst, dseq, i)
      inst
    }
    iseq
  }

  def extractFeatures(dseq: SourceSequence[Obs]): InstanceSequence = {
    if (opts.rawCache) {
      var sid = -1
      val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
        if (dseq(i).beg) sid += 1
        val inst = frep.createInstance(dseq(i), sid)
        frep.applyFeatureFns(inst, dseq, i)
      }
      InstSeq(this.asInstanceOf[TrainingSeqGen[String]], dseq.asInstanceOf[SourceSequence[String]], dseq.st, dseq.en)
    } else {
      InstSeq(extractFeaturesDirect(dseq), dseq.st, dseq.en)
    }
  }

  override def countFeatureTypes(dseq: SourceSequence[Obs]): Unit = {
    var sid = -1
    var i = 0
    while (i < dseq.length) {
      if (dseq(i).beg) sid += 1
      frep.countFeatureTypes(dseq, i)
      i += 1
    }
  }

  // note that this extraction is done over a document's worth of sequences
  // relevant for document-level feature handling
  def extractFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] = {
    CrfInstance.numLabels_=(lAlphabet.size)
    if (opts.randomFeatures || opts.randomSupportedFeatures) {
      sourcePairSeqs foreach countFeatureTypes
    }
    frep.otherIndex_=(otherIndex match { case Some(v) => v case None => -1 }) // book-keeping to tell FeatureRep
    frep.resetDisplacement // reset the displaceable feature table
    if (opts.semiCrf) {
      extractSemiCrfFeatures(sourcePairSeqs) // need to do this a bit differently for semiCRFs
    } else {
      sourcePairSeqs map extractFeatures
    }
  }
}

abstract class NonFactoredTrainingSeqGen[Obs](fr: NonFactoredFeatureRep[Obs], opts: Options) extends SeqGen[Obs](opts) {

  type FRepT = NonFactoredFeatureRep[Obs]
  this.addBeginStates_=(!opts.noBegin)
  val frep = fr
  val boundaries = opts.boundaries
  def getNumberOfFeatures = frep.faMap.size
  override def getNumberOfStates = opts.numStates match {
    case Some(v) => v case None => throw new RuntimeException("Non-factored model requires number of states to be specified")
  }

  class PrintFaMap extends cern.colt.function.LongIntProcedure {
    def apply(f: Long, i: Int) = {
      println("f: " + f + " i: " + i)
      true
    }
  }

  def extractFeatures(dseq: SourceSequence[Obs]): InstanceSequence = {
    var sid = -1
    val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
      if (dseq(i).beg) sid += 1
      val inst = frep.createInstance(dseq(i), sid)
      frep.applyFeatureFns(None, inst, dseq, i)
      inst: AbstractInstance
    }
    InstSeq(iseq, dseq.st, dseq.en)
  }

  def extractFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] = {
    if (opts.numRandomFeatures > 0) { // create instance sequences that only contain ObsSource
      sourcePairSeqs map { dseq => InstSeq(this, dseq) }
    } else
      sourcePairSeqs map extractFeatures
  }

}

abstract class DecodingSeqGen[Obs](model: Model, val decodingOpts: Options) extends SeqGen[Obs](decodingOpts) {

  import scala.collection.mutable.Map

  override val lAlphabet = model.labelAlphabet
  override val recodeAlphabet = model.labelAlphabet

  val boundaries = decodingOpts.boundaries
  def setBegin() = this.addBeginStates_=(true)

  def extractFeatures(spSeq: SourceSequence[Obs]): InstanceSequence

  /**
   * Create a deserialized object from a raw string
   * @param string  A string representing a piece of text to process
   * @return        A deserialized object
   */
  def deserializeFromRawString(string: String): DeserializationT

  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: String): Unit = seqsToFile(d, seqs, new File(f))
  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: File): Unit
  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter, close: Boolean = true): Unit

  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]): String
  def seqsToAnnotations(d: DeserializationT, seqs: Seq[InstanceSequence]): scala.collection.immutable.Map[AbstractLabel, ListBuffer[Annotation]]

}

abstract class NonFactoredDecodingSeqGen[Obs](fr: NonFactoredFeatureRep[Obs], val m: NonFactoredModel, opts: Options) extends DecodingSeqGen[Obs](m, opts) {
  type FRepT = NonFactoredFeatureRep[Obs]
  val frep = fr
  override def getNumberOfStates = opts.numStates match {
    case Some(v) => v case None => throw new RuntimeException("Non-factored model requires number of states to be specified")
  }

  def getNumberOfFeatures = frep.faMap.size
  override def getNumberOfNeuralFeatures = 0

  def extractFeatures(dseq: SourceSequence[Obs]): InstanceSequence = {
    var sid = -1
    val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
      if (dseq(i).beg) sid += 1
      val inst = frep.createInstance(dseq(i).label, dseq(i).label, sid)
      frep.applyFeatureFns(None, inst, dseq, i)
      inst: AbstractInstance
    }
    InstSeq(iseq, dseq.st, dseq.en)
  }

  def extractFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] =
    sourcePairSeqs map { dseq => extractFeatures(dseq) }

}

abstract class FactoredDecodingSeqGen[Obs](fr: DecodingFactoredFeatureRep[Obs], model: StdModel, decodingOpts: Options, preModel: Boolean = false)
  extends DecodingSeqGen[Obs](model, decodingOpts) {
  def this(m: StdModel, opts: Options, pre: Boolean = false) = this(new DecodingFactoredFeatureRep[Obs](opts, m, pre), m, opts)
  def this(model: StdModel, pre: Boolean = false) = this(model, new Options(), pre)

  type FRepT = DecodingFactoredFeatureRep[Obs]
  val frep: FRepT = fr

  def getNumberOfFeatures = -1
  override def getNumberOfNeuralFeatures = -1

  def extractFeatures(seq: SourceSequence[Obs]): InstanceSequence = {
    val iseq = Vector.tabulate(seq.length) { (i: Int) =>
      val lab = seq(i).label
      val inst = frep.createInstance(lab, lab, (-1))
      frep.applyFeatureFns(inst, seq, i)
      inst: AbstractInstance
    }
    InstSeq(iseq, seq.st, seq.en)
  }

  def extractFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] = {
    frep.resetDisplacement
    sourcePairSeqs map { extractFeatures(_) }
  }
}
