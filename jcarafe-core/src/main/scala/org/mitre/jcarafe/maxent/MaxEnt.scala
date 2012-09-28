/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent
import scala.math._
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._
import FastLoops._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Vector
import collection.mutable.ArrayBuffer
import java.io._
import IncrementalMurmurHash._

class MaxEntOptionHandler(argv: Array[String]) extends BaseOptionHandler(argv, false) {
  "--input-file" desc "Input file"
  "--input-dir" desc "Input directory"
  "--max-iters" desc "Maximum number of training iterations"
  "--gaussian-prior" desc "Gaussian prior regularizer"
  "--folds" desc "Number of folds"
  "--report" desc "Report file for x-validation results"
  "--output-file" desc "Output classifications"
  "--induced-features-file" desc "Induced features"
  "--file-processing" flag "Use file-based pre-processing"
  "--no-tags" flag "Do not attempt to process SGML/XML tags"
  "--train" flag "Training mode"
  "--model" desc "Model file"
  "--fspec" desc "Feature spec"
  "--evaluate"           desc "Evaluate decoder on gold-standard test data"  
  "--word-properties" desc "Word properties file"
  "--parallel" flag "Parallelize feature expectation computation"
  "--nthreads" desc "Number of threads to use during CLL training (for computing feature expectations)"
  "--dump-instances" desc "When in file processing mode, this option specifies a file to dump the extracted classification instances out to"
  "--weighted-feature-map" desc "Induce weighted feature map from auxillary data"
  "--ss-iters" desc "Number of iterations for self-induced feature parameterization"
  "--unlabeled-input-dir" desc "Directory containing files of unlabeled data for use with semi-supervised learning"
  "--weighted-feature-vectors" desc "Induced feature vectors"
}

class MaxEntDeserialization(val is: BufferedReader) extends Deserialization {
  def this(f: File) = this(new BufferedReader(new InputStreamReader(new DataInputStream(new FileInputStream(f)))))
  def this(s: String) = this(new BufferedReader(new StringReader(s)))
  type T = BufferedReader
  def getSlice(s: Int, e: Int) = throw new RuntimeException("UNSUPPORTED")
  def close() = is.close
}

class MaxEntInstance(label: Int, orig: Int, var maxentVec: Option[Array[CompactFeature]] = None, val srcInfo: Option[String] = None) extends AbstractInstance(label, orig, -1) {
  def this(l: Int, o: Int, mv: Array[CompactFeature]) = this(l, o, Some(mv))
  type FType = CompactFeature
  val maxentBuffer = new ArrayBuffer[CompactFeature]
  def getCompVec = throw new RuntimeException("Not implemented for efficiency")
  def getCompactVec: Array[CompactFeature] = getCacheMEVec

  private def getCacheMEVec: Array[CompactFeature] = maxentVec match {
    case None =>
      val cv = expandVec
      maxentVec = Some(cv)
      cv
    case Some(cv) => cv
  }

  override def add(ft: CompactFeature) = maxentBuffer append ft

  private def expandVec: Array[FType] = {
    maxentBuffer.toArray
  }

  def getNamedFeatureVec: Array[Array[(Long, Feature)]] = throw new RuntimeException("Unsupported method")
}

class SelfInducableMaxEntInstance(label: Int, orig: Int, maxentVec: Option[Array[CompactFeature]] = None)
  extends MaxEntInstance(label, orig, maxentVec) {

  var selfUserFeatures: Set[Long] = Set()

  override def addSelf(l: Long): Unit = selfUserFeatures += l
  override def selfUserVec: Set[Long] = selfUserFeatures

}

class FeatureId(val fnId: Long, val fclass: Long) {
  def this(s: String) = this(hash(s, 0), FeatureId.selfCode)
  def this(s: String, cl: String) = this(hash(s, 0), hash(cl, 0))
}
object FeatureId {
  val selfCode = hash("$=SELF=$", 0)
  val fMapping = new collection.mutable.HashMap[Long, String]()
  val unkCode = hash("$=BIAS=$", 0)
  var maintainMapping = false

  def apply(s: String) = s.split('|').toList match {
    case a :: b :: Nil => val fid = new FeatureId(b, a); if (maintainMapping) fMapping.update(fid.fnId, b); fid
    case a :: _ => val fid = new FeatureId(a); if (maintainMapping) fMapping.update(fid.fnId, a); fid
    case Nil => throw new RuntimeException("Unable to parse input line: " + s)
  }
}

class MaxEntFeatureType(val fid: Int) extends FeatureType(0L, false, 0)

/**
 * Some core functionality shared by trainer and decoder for MaxEnt
 */
trait MaxEntCore {

  private def denseDotProduct(offset: Int, lab: Int, denseVec: Array[Double], sparseFeatures: Seq[CompactFeature]) = {
    var r = 0.0
    var i = 0
    val sl = sparseFeatures.length
    while (i < sl) {
      val sv = sparseFeatures(i)
      r += sv(lab) * denseVec(sv.fid + offset)
      i += 1
    }
    r
  }

  /**
   * Gets the normalized scores for each class outcome for a particular instance given the current
   * parameters, <i>lambdas</i>, and the features associated with the instance, <i>sparseFeatures</i>
   */
  def classScoresNormalized(nls: Int, predNFS: Int, lambdas: Array[Double], sparseFeatures: Array[CompactFeature]) = {
    val unScores = for (i <- 0 until nls) yield { denseDotProduct(i * predNFS, i, lambdas, sparseFeatures) }
    var sum = 0.0
    val mx = unScores.foldLeft(-scala.Double.MaxValue) { (ac, v) => if (v > ac) v else ac }
    val preNorm = unScores map { v =>
      val ev = exp(v - mx);
      sum += ev; ev
    }
    preNorm map { v => v / sum }
  }
}

class DenseMaxEntWorker(override val lambdas: Array[Double], nls: Int, nfs: Int, gPrior: Double)
  extends MaxEnt(nls, nfs, gPrior) with DenseWorker

class DenseParallelMaxEnt(numPs: Int, nls: Int, nfs: Int, gPrior: Double) extends MaxEnt(nls, nfs, gPrior)
  with ParCrf[DenseMaxEntWorker] with CondLogLikelihoodLearner {

  def getWorker(lambdas: Array[Double], nls: Int, nfs: Int, ss: Int, gPrior: Double) =
    new DenseMaxEntWorker(lambdas, nls, nfs, gPrior)

  override def getGradient(seqAccessor: AccessSeq): Option[Double] = getGradient(numPs, seqAccessor)
}

/*
 * This class extends a DenseCrf, redefining the computation of the log-likelihood and its gradient.
 * The major change here is that we add parameters for unsupported features (i.e. those that do not appear
 * in the training data with a particular class label). Most of the CRF infrastructure is actually
 * by-passed to make things simpler and more efficient for MaxEnt.
 * 
*/
class MaxEnt(nls: Int, nfs: Int, gPrior: Double) extends DenseCrf(nls, nfs, 1, gPrior) with MaxEntCore with CondLogLikelihoodLearner {

  val predNFS = nfs / nls

  override def regularize() = {
    var i = 0
    var llMod = 0.0
    while (i < lambdas.length) {
      val li = lambdas(i)
      gradient(i) = li * invSigSqr
      llMod += (li * li * invSigSqr) / 2.0
      i += 1
    }
    llMod
  }

  def gradOfElement(el: AbstractInstance) = {
    val instFeatures: Array[CompactFeature] = el.getCompactVec
    val trueLabel = el.label
    val scores = classScoresNormalized(nls, predNFS, lambdas, instFeatures).toArray
    var k = 0
    val il = instFeatures.length
    while (k < il) {
      var l = 0
      val inst = instFeatures(k)
      val fid = inst.fid
      while (l < nls) {
        val offset = l * predNFS
        val actualIndex = fid + offset
        val v = inst(l)
        if (l == trueLabel) {
          gradient(actualIndex) -= v
        }
        gradient(actualIndex) += scores(l) * v
        l += 1
      }
      k += 1
    }
    log(scores(trueLabel))
  }

  override def getGradient(seqAccessor: AccessSeq): Option[Double] = getGradient(true, seqAccessor)
  override def getGradient(l2: Boolean, seqAccessor: AccessSeq): Option[Double] = {
    val sl = seqAccessor.length
    var logLi = if (l2) regularize() else 0.0
    forIndex(sl) { i =>
      val el = seqAccessor.accessSingleInstance(i)
      val gr = gradOfElement(el)
      logLi -= gr
    }
    Some(logLi)
  }
}

abstract class SparseMaxEnt(nls: Int, nfs: Int, opts: Options) extends StochasticCrf(nls, nfs, 1, opts) with MaxEntCore {

  val predNFS = nfs / nls

  def gradOfElement(el: AbstractInstance) = {
    val instFeatures: Array[CompactFeature] = el.getCompactVec
    val trueLabel = el.label
    val scores = classScoresNormalized(nls, predNFS, lambdas, instFeatures).toArray
    var k = 0
    val il = instFeatures.length
    while (k < il) {
      var l = 0
      val inst = instFeatures(k)
      val fid = inst.fid
      while (l < nls) {
        val offset = l * predNFS
        val v = inst(l)
        val actualIndex = fid + offset
        val gref = gradient.get(actualIndex) match {
          case Some(v) => v
          case None =>
            val nv = new DoubleCell(0.0, 0.0)
            gradient += ((actualIndex, nv))
            nv
        }
        if (l == trueLabel) {
          gref.g_=(gref.g + v)
        }
        gref.e_=(gref.e + scores(l) * v)
        l += 1
      }
      k += 1
    }
    log(scores(trueLabel))
  }

  override def getGradient(seqAccessor: AccessSeq) = getGradient(true, seqAccessor)
  def getGradient(l2: Boolean, seqAccessor: AccessSeq) = {
    val asize = batchSize
    val sl = seqAccessor.length
    var gradNormalizer = 0.0
    for (i <- curPos until curPos + asize) {
      val j = i % sl
      gradient.foreach { case (k, v) => v.e_=(0.0) } // reset expectations to zero
      val el = seqAccessor.accessSingleInstance(j)
      val gr = gradOfElement(el)
      for ((k, cell) <- gradient) {
        cell.g_=(cell.g - cell.e)
      }
    }
    curPos += asize
    None
  }
}

class MaxEntMemoryAccessSeq(iseqs: Seq[InstanceSequence]) extends MemoryAccessSeq(iseqs) {

  override def accessSingleInstance(i: Int): AbstractInstance = apply(0)(i)
  override def length = apply(0).length // length of the single sequence
  override def splitAccessor(n: Int): Seq[AccessSeq] = {
    assert(seqs.length == 1) // should just be a single sequence in this case
    val seq = seqs(0)
    val ns = if ((seq.length % n) == 0) seq.length / n else (seq.length / n) + 1
    for (j <- 0 until n) yield {
      new MaxEntMemoryAccessSeq(Seq(InstSeq(seq.iseq.slice(j * ns, (seq.length min ((j + 1) * ns))))))
    }
  }
}

/*
 * MaxEnt decoding is very simple. No need for Viterbi, but we still re-use some of the data
 * structures.  In particular, we still use a sequence of instances, but we assume there is
 * not inter-dependence between instances (i.i.d.). 
*/
class MaxEntDecodingAlgorithm(crf: CoreModel) extends DecodingAlgorithm(crf) with MaxEntCore {

  val predNFS = crf.nfs / crf.nls

  def numLabels = crf.nls
  def getCopyOf = new MaxEntDecodingAlgorithm(this.crf)

  /**
   * Each instance is assigned the label with the highest class probability
   */
  def assignBestSequence(iseq: Seq[AbstractInstance]) = { iseq foreach classifyInstance; 0.0 }

  def classifyInstance(el: AbstractInstance): Int = {
    val scores = classScoresNormalized(crf.nls, predNFS, crf.params, el.getCompactVec)
    val scoresZipped = scores.toList.zipWithIndex
    scoresZipped foreach { sc => el.setConditionalProb(sc._2, sc._1) }
    val r = scoresZipped.foldLeft((0.0, 0)) { (ac, v) => if (v._1 > ac._1) v else ac }._2
    el.label = r // make sure to set the label for subsequent scoring, etc.
    r
  }

  def getInstanceDistribution(el: AbstractInstance): List[(Double, Int)] = {
    val scores = classScoresNormalized(crf.nls, predNFS, crf.params, el.getCompactVec)
    scores.toList.zipWithIndex
  }
}

class MaxEntTrainingSeqGen(opts: Options) extends SeqGen[List[(FeatureId, Double)]](opts) with MaxEntSeqGenAttVal {

  val frep = new MEFRep[List[(FeatureId, Double)]]
  val boundaries = opts.boundaries

  def getNumberOfFeatures = frep.fMap.size * getNumberOfStates
  var cnt = 0
  def createInstancesOnDisk: Unit = {}

}

class FileBasedMaxEntTrainingSeqGen(opts: Options) extends MaxEntTrainingSeqGen(opts) with MaxEntSeqGenAttValFromFileProcessor {
  val subSeqGen = new TrainingSeqGen[String](opts) with TextSeqGen
}

class DiskBasedMaxEntTrainingSeqGen(opts: Options) extends MaxEntTrainingSeqGen(opts) {

  import MESerializations._

  def writeInstance(odir: String, inst: MaxEntInstance) = {
    val ofile = new java.io.File(odir + "/" + cnt.toString)
    sbinary.Operations.toFile(inst)(ofile)
  }

  protected def toDiskInstances(inReader: DeserializationT): Unit = {
    val instr = inReader.is
    var l = instr.readLine()
    var counter = 0
    val odirStr = opts.diskCache match { case Some(dir) => dir case None => throw new RuntimeException("Expecting cache directory") }
    while (l != null) {
      buildInstance(l) match {
        case Some(inst) => writeInstance(odirStr, inst); cnt += 1
        case None =>
      }
      counter += 1
      if ((counter % 1000) == 0) print(".")
      if ((counter % 5000) == 0) println(" " + counter + " instances read in")
      l = instr.readLine()
    }
    println("\n...finished reading in dataset...")
    inReader.close()
  }

  override def createInstancesOnDisk: Unit = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new File(dirStr)
        dir.listFiles.toSeq filter
          { f: File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } } foreach
          { f: File => toDiskInstances(deserializeFromFile(f)) }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            toDiskInstances(deserializeFromFile(f))
          case None =>
            throw new RuntimeException("Expecting input file")
        }
    }
  }

}

class MEFRep[Obs](val m: Option[MaxEntModel] = None) extends FeatureRep[Obs](false) {
  var inducedFeatureMap: Option[InducedFeatureMap] = None
  def createSource(l: Int, o: Obs, b: Boolean, i: Option[Map[String, String]]) = new ObsSource(l, o, b, None)
  def createSource(l: Int, o: Obs, b: Boolean) = new ObsSource(l, o, b, None)
  def createInstance(l: Int, o: Int, sId: Int) = new MaxEntInstance(l, o)
  def createInstance(l: Int, o: Int) = new MaxEntInstance(l, o)
  def getFeatureSetName: String = ""
  def getLexicon: Option[BloomLexicon] = None
  def getWordProps: Option[WordProperties] = None
  def getWordScores: Option[WordScores] = None
  def getInducedFeatureMap: Option[InducedFeatureMap] = inducedFeatureMap
  def this(m: MaxEntModel) = this(Some(m))

  val fMap = m match {
    case Some(m) =>
      val mm = m.fsetMap
      mm.fixed_=(true)
      mm
    case None => new Alphabet[Long]()
  }
  val unkCode = hash("$=BIAS=$", 0)

  def addMEFeature(inst: MaxEntInstance, fname: Long, vl: Double, clWts: Option[Array[Double]] = None): Unit = {
    val fid = fMap.update(fname)
    if (fid >= 0)
      inst add (new CompactFeature(vl, fid, clWts))
  }

  def createMEInstance(l: Int, o: Int): MaxEntInstance = new MaxEntInstance(l, o)
  def createMEInstance(l: Int, o: Int, src: String) = new MaxEntInstance(l,o,srcInfo=Some(src))

}

abstract class MaxEntDecodeSeqGen(m: MaxEntModel, opts: Options) extends DecodingSeqGen[List[(FeatureId, Double)]](m, opts) with MaxEntSeqGenAttVal {

  val frep = new MEFRep[List[(FeatureId, Double)]](m)

  def getNumberOfFeatures = frep.fMap.size * getNumberOfStates

}

class FileBasedMaxEntDecodeSeqGen(m: MaxEntModel, opts: Options) extends DecodingSeqGen[List[(FeatureId, Double)]](m, opts) with MaxEntSeqGenAttVal with MaxEntSeqGenAttValFromFileProcessor {
  
  //val subSeqGen = new FactoredDecodingSeqGen[String](m, opts) with TextSeqGen
  val subSeqGen = new TrainingSeqGen[String](opts) with TextSeqGen

  def getNumberOfFeatures = frep.fMap.size * getNumberOfStates
  var cnt = 0
  def createInstancesOnDisk: Unit = {}

  val frep = new MEFRep[List[(FeatureId, Double)]](m)

} 

trait MaxEntSeqGenCore[Obs] extends SeqGen[Obs] {

  type DeserializationT = MaxEntDeserialization

  def toSources(d: DeserializationT) = throw new RuntimeException("UNIMPLEMENTED")

  def deserializeFromFile(file: String): DeserializationT = {
    new MaxEntDeserialization(new java.io.File(file))
  }

  def deserializeFromString(s: String): DeserializationT = {
    new MaxEntDeserialization(s)
  }

  def initialize(): Unit = throw new RuntimeException("Unimplemented")

  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT =
    throw new RuntimeException("UNSUPPORTED")
  //new MaxEntDeserialization(Vector.tabulate(seqs(0).length){(x:Int) => invLa(seqs(0).iseq(x).label).labelString})

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter): Unit =
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Xml mode")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File): Unit = {
    val ostr = new java.io.FileOutputStream(f)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    seqs(0).iseq foreach { e =>
      var i = 0
      while (i < getNumberOfStates) {
        if (i > 0) os.write("\t")
        os.write(invLa(i) + ":" + e.conditionalProb(i));
        i += 1
      }
      os.write("\n")
    }
    os.close()
    d.close()
  }

  def deserializeFromTokenSeq(seq: Seq[String]): DeserializationT = throw new RuntimeException("Unimplemented")

  def deserializeFromRawString(string: String): DeserializationT = throw new RuntimeException("Unimplemented")

  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]): String =
    throw new RuntimeException("Unimplemented")

  def seqsToAnnotations(d: DeserializationT, seqs: Seq[InstanceSequence]): Map[AbstractLabel, ListBuffer[Annotation]] =
    throw new RuntimeException("Unsupported method: seqsToAnnotations")

  def seqsToAttributedDeserialization(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT =
    throw new RuntimeException("Unsuppoted method: seqsToAttributedDeserialization")

}

trait MaxEntSeqGen[Obs] extends MaxEntSeqGenCore[Obs] {

  type FRepT = MEFRep[Obs]

  val codec = scala.io.Codec("utf-8")

  val unkCode = hash("$=BIAS=$", 0)
  val selfCode = hash("$=SELF=$", 0)
  var ufset: Set[Long] = Set()
  var dumpFVecStream: Option[java.io.OutputStreamWriter] = None

  def setFVecStream(f: java.io.File) = {
    dumpFVecStream = Some(new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(f)), "UTF-8"))
  }

  def closeFVecStream() = dumpFVecStream foreach { _.close }

  def addInFeatures(inst: MaxEntInstance, src: ObsSource[List[(FeatureId, Double)]]): Unit = {
    val yp = src.label
    frep.addMEFeature(inst, unkCode, 1.0)
    src.obs foreach { case (l, v) => frep.addMEFeature(inst, l.fnId, v) }
    frep.getInducedFeatureMap match {
      case Some(im) =>
        val nls = im.vecSize
        var mapToSmoothedDistribution: Map[Long, Array[Double]] = Map()
        src.obs foreach {
          case (l, v) =>
            im.getWeightedFeatureVector(l.fnId) match {
              case Some(vec) =>
                ufset += l.fclass
                val ar = mapToSmoothedDistribution.get(l.fclass) match {
                  case Some(a) => a
                  case None =>
                    val ar = Array.fill(nls)(1.0)
                    mapToSmoothedDistribution += ((l.fclass, ar))
                    ar
                }
                forIndex(nls) { i => ar(i) += vec(i) }
              case None =>
            }
        }
        mapToSmoothedDistribution foreach {
          case (l, ar) =>
            val s = ar.foldLeft(0.0)(_ + _)
            forIndex(ar.length) { i => ar(i) = ar(i) / s }
            dumpFVecStream match {
              case Some(os) =>
                forIndex(ar.length) { i =>
                  val nid = IncrementalMurmurHash.mix(i, l)
                  os.write(nid.toString)
                  os.write(':')
                  os.write(ar(i).toString)
                  os.write(' ')
                }
              case None =>
            }
            frep.addMEFeature(inst, l, 1.0, Some(ar))
        }
        dumpFVecStream foreach { s => s.write('\n') }
      case None =>
    }
  }

  override def createSeqsFromFiles: Seq[InstanceSequence] = createInstancesDirectly
  protected def toInstances(d: DeserializationT): InstanceSequence

  protected def createInstancesDirectly: Seq[InstanceSequence] = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new File(dirStr)
        dir.listFiles.toSeq filter
          { f: File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } } map
          { f: File => toInstances(deserializeFromFile(f)) }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            Seq(toInstances(deserializeFromFile(f)))
          case None =>
            throw new RuntimeException("Expecting input file")
        }
    }
  }
}

trait MaxEntSeqGenAttVal extends MaxEntSeqGen[List[(FeatureId, Double)]] {

  override def extractFeatures(sourcePairSeqs: Seqs): Seq[InstanceSequence] = {
    sourcePairSeqs map extractFeatures
  }

  def extractFeatures(obs: ObsSource[List[(FeatureId, Double)]]): AbstractInstance = {
    val inst = frep.createInstance(obs.label, obs.label)
    addInFeatures(inst, obs)
    inst: AbstractInstance
  }

  def extractFeatures(dseq: SourceSequence[List[(FeatureId, Double)]]): InstanceSequence = {
    val iseq = Vector.tabulate(dseq.length) { (i: Int) =>
      val inst = frep.createInstance(dseq(i).label, dseq(i).label, i)
      addInFeatures(inst, dseq(i))
      inst: AbstractInstance
    }
    InstSeq(iseq)
  }

  protected def buildInstance(l: String): Option[MaxEntInstance] = {
    if (l.length > 2) { // just skip short/empty lines
      l.split(" ").toList match {
        case lab :: fs =>
          val src = createSource(SLabel(lab), (fs map { el =>
            el.split(":").toList match {
              case a :: b :: Nil => (FeatureId(a), b.toDouble)
              case a :: _ => (FeatureId(a), 1.0)
              case Nil => throw new RuntimeException("Feature vector parse failed")
            }
          }), false)
          val inst = frep.createMEInstance(src.label, src.label)
          addInFeatures(inst, src)
          Some(inst)
        case _ => None
      }
    } else None
  }

  protected def toInstances(inReader: DeserializationT): InstanceSequence = {
    val instr = inReader.is
    var l = instr.readLine()
    val tmpBuf = new scala.collection.mutable.ListBuffer[MaxEntInstance]
    var counter = 0
    while (l != null) {
      buildInstance(l) match {
        case Some(inst) =>
          tmpBuf += inst
          counter += 1
          if ((counter % 1000) == 0) print(".")
          if ((counter % 5000) == 0) println(" " + counter + " instances read in")
        case None =>
      }
      l = instr.readLine()
    }
    println("\n...finished reading in dataset...")
    inReader.close()
    InstSeq(tmpBuf.toIndexedSeq)
  }
}

class DiskBasedMaxEntTrainer(opts: MEOptions) extends MaxEntTrainer(opts) with LinearCRFTraining[List[(FeatureId, Double)]] {
  def this() = this(new MEOptions(Array(), new MaxEntOptionHandler(Array())))
  override val sGen: TrSeqGen = new DiskBasedMaxEntTrainingSeqGen(opts)
  import MaxEntSerializer._

  def trainModelWithDiskAccess(me: Trainable) = {
    val dc = opts.diskCache match { case Some(dir) => dir case None => "/tmp" }
    println("training with disk access over: " + sGen.cnt + " instances")
    val accessSeq = new MaxEntDiskAccessSeq(dc, 0, (sGen.cnt - 1))
    val coreModel = me.train(accessSeq, opts.maxIters)
    val m = new MaxEntModel(sGen.getLAlphabet, coreModel, sGen.frep.fMap)
    writeModel(m, new java.io.File(opts.model.get))
  }

  override def trainModel(m: Trainable, seqs: Seq[InstanceSequence], modelIterFn: Option[(CoreModel,Int) => Unit] = None) = trainModelWithDiskAccess(m)

  override def train = {
    sGen.createInstancesOnDisk // build instances and cache to disk
    val me =
      if (opts.parallel) {
        val numPs = opts.numThreads match {
          case None => Runtime.getRuntime.availableProcessors * 4 / 5 // leave a CPU or two free
          case Some(n) => n
        }
        println(">> Initiating Parallel Training using " + numPs + " processors <<\n")
        new DenseParallelMaxEnt(numPs, sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts.gaussian)
      } else if (opts.psa) {
        println("PSA training....")
        if (opts.l1)
          new SparseMaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts) with PsaLearnerWithL1
        else
          new SparseMaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts) with PsaLearner
      } else new MaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts.gaussian) with CondLogLikelihoodLearner
    trainModelWithDiskAccess(me)
  }

  override def xValidate = {
    throw new RuntimeException("XValidation currently unsupported with disk-based feature caching ")
  }
}

class MaxEntTrainer(opts: MEOptions) extends Trainer[List[(FeatureId, Double)]](opts) with LinearCRFTraining[List[(FeatureId, Double)]] {
  import MaxEntSerializer._
  def this() = this(new MEOptions(Array(), new MaxEntOptionHandler(Array())))
  type TrSeqGen = MaxEntTrainingSeqGen
  val sGen: TrSeqGen = if (opts.fileBased) new FileBasedMaxEntTrainingSeqGen(opts) else new MaxEntTrainingSeqGen(opts)
  override def trainModel(me: Trainable, seqs: Seq[InstanceSequence],modelIterFn: Option[(CoreModel, Int) => Unit] = None) = {
    if (opts.dumpInstances.isDefined) {
      val ofile = new java.io.FileWriter(opts.dumpInstances.get)
      seqs foreach { iseq =>
        iseq.iseq foreach { ai =>
          ofile.write(ai.label.toString)
          ai.getCompactVec foreach { cf => ofile.write(' '); ofile.write(cf.fid.toString) }
          ofile.write('\n')
        }
      }
      ofile.close()
      println("\n.. Completed writing training instances to file: " + opts.dumpInstances.get)
    } else {
      val accessSeq = new MaxEntMemoryAccessSeq(seqs)
      val coreModel = me.train(accessSeq, opts.maxIters)
      val m = new MaxEntModel(sGen.getLAlphabet, coreModel, sGen.frep.fMap, sGen.getInducedFeatureMap)
      writeModel(m, new java.io.File(opts.model.get))
    }
  }
  
  def getMeEstimator = {
    if (opts.parallel) {
        val numPs = opts.numThreads match {
          case None => Runtime.getRuntime.availableProcessors * 4 / 5 // leave a CPU or two free
          case Some(n) => n
        }
        println(">> Initiating Parallel Training using " + numPs + " processors <<\n")
        new DenseParallelMaxEnt(numPs, sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts.gaussian)
      } else if (opts.psa) {
        if (opts.l1)
          new SparseMaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts) with PsaLearnerWithL1
        else
          new SparseMaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts) with PsaLearner
      } else new MaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, opts.gaussian) with CondLogLikelihoodLearner
  }
  
  override def train = {
    val seqs: Seq[InstanceSequence] = sGen.createSeqsFromFiles
    val me = getMeEstimator
    trainModel(me, seqs)
  }

  def xValidate = {
    val seqs: Seq[InstanceSequence] = sGen.createSeqsFromFiles
    val evaluator = new Evaluator(opts, sGen)
    evaluator.addInstances(seqs(0))
    evaluator.produceReport(opts.xValFolds.get.toInt, new java.io.File(opts.report.get))
  }
}

class MaxEntDecoder(decodingOpts: MEOptions, val model: MaxEntModel) extends Decoder[List[(FeatureId, Double)]](decodingOpts) {
  type M = MaxEntModel
  def this(m: MaxEntModel) = this(new MEOptions, m)
  //val sGen = new MaxEntDecodeSeqGen(model, decodingOpts) with SeqGenScorer[List[(FeatureId, Double)]]
  val sGen = 
    if (decodingOpts.fileBased)
      new FileBasedMaxEntDecodeSeqGen(model, decodingOpts) with SeqGenScorer[List[(FeatureId, Double)]]
    else new MaxEntDecodeSeqGen(model, decodingOpts) with SeqGenScorer[List[(FeatureId, Double)]]
  setDecoder(true)
  
  protected def gatherFeatures(seqs: Seq[InstanceSequence]) : Set[String] = 
    seqs.foldLeft(Set():Set[String]){(cs,seq) => seq.iseq.foldLeft(cs){(cs1,se) => se.userVec.foldLeft(cs1){_ + _.getName}}}
  
  protected def mapToMaxEntInstance(lab: String, fs: Seq[InstanceSequence]) = { 
    val meFs : Set[String] = gatherFeatures(fs)
    val src = sGen.createSource(SLabel(lab), meFs.toList map { fn => (new FeatureId(fn),1.0)})
    val inst = sGen.frep.createMEInstance(src.label,src.label)
    //frep.addMEFeature(inst, unkCode, 1.0)
    //src.obs foreach {case (l,v) => frep.addMEFeature(inst,l.fnId,v)}
    sGen.addInFeatures(inst,src)
    inst
  }
  
  val subSeqGen = new TrainingSeqGen[String](decodingOpts) with TextSeqGen

  def decodeFileBased() = {
    val decoder = new MaxEntDecodingAlgorithm(model.crf) 
    val seq = sGen.createSeqsFromFiles
    decodingOpts.outputFile match {
      case None =>
        seq foreach {s => decoder.assignBestSequence(s) }
        sGen.evaluateSequences(seq)
      case Some(ofile) =>
        val o = new java.io.File(ofile)
        val os = new java.io.PrintWriter(o)
        val invLa = sGen.invLa
        seq foreach {s =>
          decoder.assignBestSequence(s)
          s.iseq foreach {ai =>
            ai match {
              case meI : MaxEntInstance => 
                os.write(meI.srcInfo.get)
                for (i <- 0 until sGen.getNumberOfStates) {
                  os.write("\t" + invLa(i) + ":" + meI.conditionalProb(i))
                }
                os.write('\n')
              case _ =>
            }
            }
          }
        os.close
    }
  }
  
  override def runDecoder(deser: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]) = {
    val instr = deser.is
    var l = instr.readLine()
    val tmpBuf = new scala.collection.mutable.ListBuffer[MaxEntInstance]
    while (l != null) {
      if (l.length > 2) {
        l.split(" ").toList match {
          case lab :: fs =>
            val src = sGen.createSource(SLabel(lab), (fs map { el =>
              el.split(":").toList match {
                case a :: b :: Nil => (FeatureId(a), b.toDouble)
                case a :: _ => (FeatureId(a), 1.0)
                case Nil => throw new RuntimeException("Feature vector parse failed")
              }
            }), false)
            val inst = sGen.frep.createMEInstance(src.label, src.label)
            sGen.addInFeatures(inst, src)
            tmpBuf += inst
          case _ =>
        }
      }
      l = instr.readLine()
    }
    val seq = new MemoryInstanceSequence(tmpBuf.toIndexedSeq)
    decoder.assignBestSequence(seq)
    decodingOpts.evaluate match {
      case Some(_) =>
        sGen.evaluateSequences(Seq(seq))
      case None =>
        outFile match {
          case Some(outFile) => sGen.seqsToFile(deser, Seq(seq), new java.io.File(outFile))
          case None => throw new RuntimeException("Expected output directory")
        }
    }

  }

  override def decodeToAnnotations(s: String): Array[Annotation] = throw new RuntimeException("Unavailable method")
  
  override def decode() = {
    if (decodingOpts.fileBased) decodeFileBased()
    else decodeStd()
  }
  
  def decodeStd() = {
    val decoder = new MaxEntDecodingAlgorithm(model.crf)
    decodingOpts.inputDir match {
      case Some(dirStr) =>
        val pat = decodingOpts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new java.io.File(dirStr)
        val odir = decodingOpts.outputDir
        val fs =
          dir.listFiles filter
            { f: java.io.File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } }
        val osuffix = decodingOpts.outSuffix match { case Some(o) => o case None => "" }
        fs foreach { f =>
          val ofile = decodingOpts.outputDir match { case Some(d) => Some(d + "/" + f.getName + osuffix) case None => None }
          val deser = sGen.deserializeFromFile(f)
          runDecoder(deser, decoder, ofile)
        }
      case None =>
        decodingOpts.inputFile match {
          case Some(f) =>
            val deser = sGen.deserializeFromFile(f)
            runDecoder(deser, decoder, decodingOpts.outputFile)
          case None => throw new RuntimeException("Expecting input file or input directory")
        }
    }
  }

}

object MaxEntDecoder {
  import MaxEntSerializer._
  def apply(s: String) = new MaxEntDecoder(readModel(s.getBytes))
  def apply(m: MaxEntModel) = new MaxEntDecoder(m)
  def apply(decodingOpts: MEOptions) = new MaxEntDecoder(decodingOpts, readModel(new java.io.File(decodingOpts.model.get)))
}

class MEOptions(override val argv: Array[String], override val optHandler: MaxEntOptionHandler, proc: Boolean = true) extends Options(argv, optHandler, proc) {
  def this() = this(Array(), new MaxEntOptionHandler(Array()))
  val fileBased: Boolean = optHandler.check("--file-processing")
  val dumpInstances = optHandler.get("--dump-instances")

  override def copy(): MEOptions = {
    val no = new MEOptions(argv, optHandler, false)
    setInto(no)
    no
  }
}

object MaxEntClassifier {

  def apply(argv: Array[String]) = {
    val handler = new MaxEntOptionHandler(argv)
    val opts = new MEOptions(argv, handler)
    new MaxEntClassifier(opts)
  }
}

class MaxEntClassifier(val opts: MEOptions, val imap: Option[InducedFeatureMap] = None) {
  def this(argv: Array[String]) = this(new MEOptions(argv, new MaxEntOptionHandler(argv)))

  def printUsage() = println("java -cp ..jcarafe-...jar org.mitre.jcarafe.maxent.ME  <options>")

  def setFeatureMap(trainer: MaxEntTrainer) = {
    trainer.sGen.frep.getInducedFeatureMap match {
      case None => trainer.sGen.frep.inducedFeatureMap_=(imap)
      case Some(fm) => println("Induced feature map already 'set' (contains " + fm.hmap.size + " entires)")
    }
  }

  def process(printVecs: Boolean = false) = {
    if (opts.train) {
      val trainer = opts.diskCache match {
        case Some(_) => new DiskBasedMaxEntTrainer(opts)
        case None => new MaxEntTrainer(opts)
      }
      setFeatureMap(trainer)
      if (printVecs) {
        trainer.sGen.setFVecStream(new java.io.File(opts.inducedFVecsFile.get))
      }
      /*
      opts.rewriteInducedFeatures match {
	case Some(ofile) =>
	  val deser = deserializeFromFile(opts.inputFile.get)
	  val insts = trainer.toInstances(deser)
	  // need to get the induced features out and print em
	case None =>
	*/
      trainer.train()
      println("\n...Finished training...")
      if (printVecs)
        trainer.sGen.closeFVecStream
    } else if (opts.xValFolds match { case Some(_) => true case None => false }) {
      val trainer = new MaxEntTrainer(opts)
      trainer.xValidate
    } else {
      val decoder = MaxEntDecoder(opts)
      decoder.sGen.frep.inducedFeatureMap_=(decoder.model.inducedMap)
      decoder.decode()
      val eval = opts.evaluate match { case Some(_) => true case None => false }
      if (eval) decoder.sGen.getAccuracy
      ()
    }
  }
}

object MaxEntClassifierMain {

  def main(argv: Array[String]): Unit = {
    val me = new MaxEntClassifier(argv)
    me.process()
  }
}

class RuntimeMaxEntTrainer(opts: Options, var gp: Double = 10.0) extends MaxEntTrainer {
  def this(gp: Double) = this(new Options(), gp)
  import scala.collection.JavaConversions._
  import MaxEntSerializer._

  val seqTbl = new collection.mutable.HashMap[Int, ObsSource[List[(FeatureId, Double)]]]

  var idCounter = 0

  def getObsSeqs = {
    seqTbl map { case (k, v) => v }
  }

  def trainModelToString(me: Trainable, seqs: Seq[InstanceSequence]): String = {
    val m = train(me, seqs)
    new String(serializeAsBytes(m))
  }

  def train(me: Trainable, seqs: Seq[InstanceSequence]): MaxEntModel = {
    seqs foreach {iseq => iseq.iseq foreach {ai => ai.label = ai.orig}}  // make sure the label is set ot original label here
    val accessSeq = new MaxEntMemoryAccessSeq(seqs)
    val coreModel = me.train(accessSeq, opts.maxIters)
    new MaxEntModel(sGen.getLAlphabet, coreModel, sGen.frep.fMap, sGen.getInducedFeatureMap)
  }

  def batchTrain: String = new String(serializeAsBytes(batchTrainToModel))

  def batchTrainToModel: MaxEntModel = batchTrainToModel(getObsSeqs.toSeq)

  def batchTrainToModel(obsSeq: Seq[ObsSource[List[(FeatureId, Double)]]]): MaxEntModel = {
    val seqs = sGen.extractFeatures(new SourceSequence[List[(FeatureId, Double)]](obsSeq))
    val me = new MaxEnt(sGen.getNumberOfStates, sGen.getNumberOfFeatures, gp) with CondLogLikelihoodLearner
    train(me, Seq(seqs))
  }

  def batchTrainToModel(seqGen: MaxEntTrainingSeqGen, instSeq: InstanceSequence): MaxEntModel = {
    val me = new MaxEnt(seqGen.getNumberOfStates, seqGen.getNumberOfFeatures, gp) with CondLogLikelihoodLearner
    train(me, Seq(instSeq))
  }

  def addInstance(id: Int, l: String, fs: java.util.List[String]): Unit =
    seqTbl.update(id, sGen.createSource(SLabel(l), fs.toList.map { f => (FeatureId(f), 1.0) }))

  def addValuedInstance(id: Int, l: String, fs: java.util.List[(String, java.lang.Double)]): Unit =
    seqTbl.update(id, sGen.createSource(SLabel(l), fs.toList.map { case (f, v) => (FeatureId(f), v.asInstanceOf[Double]) }))

  def addValuedInstance(l: String, fs: java.util.List[(String, java.lang.Double)]): Unit = {
    addValuedInstance(idCounter, l, fs)
    idCounter += 1
  }

  def addInstance(l: String, fs: java.util.List[String]): Unit = {
    addInstance(idCounter, l, fs)
    idCounter += 1
  }

  def addInstance(id: Int, l: String, fs: Set[String]): Unit =
    seqTbl.update(id, sGen.createSource(SLabel(l), fs.toList.map { f => (FeatureId(f), 1.0) }))

  def addInstance(l: String, fs: Set[String]): Unit = {
    addInstance(idCounter, l, fs)
    idCounter += 1
  }

}

class RuntimeMaxEntDecoder(m: MaxEntModel) extends MaxEntDecoder(m) {

  import collection.JavaConverters._

  val meDecoder = new MaxEntDecodingAlgorithm(model.crf)

  def numLabels = meDecoder.numLabels

  def decodeInstanceAsDistribution(fs: java.util.List[String]): java.util.List[(String, java.lang.Double)] =
    decodeInstanceAsDistribution(fs.asScala.toList.map { (s: String) => (s, 1.0) })

  def decodeInstanceAsDistribution(fs: Set[String]): java.util.List[(String, java.lang.Double)] =
    decodeInstanceAsDistribution(fs.toList.map { (_, 1.0) })

  def decodeInstanceAsDistribution(lfs: List[(String, Double)]): java.util.List[(String, java.lang.Double)] = {
    val n_lfs = lfs map { case (f, v) => (FeatureId(f), (v: Double)) }
    val dist: List[(Double, Int)] = meDecoder.getInstanceDistribution(sGen.extractFeatures(sGen.createSource(SLabel("UNK"), n_lfs)))
    val r = dist map { case (v, li) => (sGen.invLa(li) match { case SLabel(l) => l case _ => "UNK" }, new java.lang.Double(v)) }
    r.asJava: java.util.List[(String, java.lang.Double)]
  }

  def decodeValuedInstanceAsDistribution(fs: java.util.List[(String, java.lang.Double)]): java.util.List[(String, java.lang.Double)] =
    decodeInstanceAsDistribution(fs.asScala.toList.map { case (s, v) => (s, v.asInstanceOf[Double]) })

  def decodeInstance(fs: java.util.List[String]): String = decodeInstance(fs.asScala.toList.map { f => (FeatureId(f), 1.0) })

  def decodeValuedInstance(fs: java.util.List[Tuple2[String, java.lang.Double]]): String =
    decodeInstance(fs.asScala.toList.map { case (f, v) => (FeatureId(f), v.asInstanceOf[Double]) })

  def decodeInstance(fs: Set[String]): String = decodeInstance(fs.toList.map { f => (FeatureId(f), 1.0) })

  def decodeInstance(i: AbstractInstance): AbstractLabel = sGen.invLa(meDecoder.classifyInstance(i))

  def decodeInstance(lfs: List[(FeatureId, Double)]): String = {
    val labReturn = decodeInstanceRaw(lfs)
    val lab = sGen.invLa(labReturn) match { case SLabel(l) => l case _ => throw new RuntimeException("Unexpected label type") }
    lab
  }

  def decodeInstanceRaw(lfs: List[(FeatureId, Double)]): Int = decodeSrc(sGen.createSource(SLabel("UNK"), lfs))

  def decodeSrc(src: ObsSource[List[(FeatureId, Double)]]): Int = meDecoder.classifyInstance(sGen.extractFeatures(src))

  def decodeSrcToLabel(src: ObsSource[List[(FeatureId, Double)]]): AbstractLabel = decodeInstance(sGen.extractFeatures(src))

}

object RuntimeMaxEntDecoder {
  import MaxEntSerializer._
  def apply(m: String) = new RuntimeMaxEntDecoder(readModel(m.getBytes))
  def apply(m: java.io.File) = new RuntimeMaxEntDecoder(readModel(m))
  def apply(m: MaxEntModel) = new RuntimeMaxEntDecoder(m)
}

