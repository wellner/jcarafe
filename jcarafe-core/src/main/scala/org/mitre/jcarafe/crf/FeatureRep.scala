/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.IntMap
import scala.collection.immutable.Stack
import org.mitre.jcarafe.util._
import cern.colt.map.OpenLongIntHashMap;
import cern.colt.map.OpenLongObjectHashMap;

abstract class FeatureCore {
  def getName: String
  def value: Double
}

class CompactFeature(val v: Double, val fid: Int, val classLabelWeights: Option[Array[Double]] = None) extends FeatureCore {
  def apply(i: Int) = classLabelWeights match { case Some(vec) => vec(i) case None => v }
  def value = v
  def getName = ""
}

/*
 * represents low-level features - those actually used in the inner
 * loops of decoders and estimators
 * @param prv - integer label of previous position
 * @param cur - integer label of current position
 * @param fid - integer feature id
 * @param value - value of feature (e.g. 1.0 for binary features that are 'present')
 * @param nfid - feature id for neural features
 * @author Ben Wellner
*/
class Feature(val prv: Int, val cur: Int, val fid: Int, val nfid: Int = -1) extends FeatureCore {

  import IncrementalMurmurHash._
  def this(prv: Int, cur: Int, fid: Int) = this(prv, cur, fid, -1)
  def value = 1.0 // 1.0 by default
  def getName = fid.toString
  override def equals(other: Any): Boolean = other match {
    case that: Feature => fid == that.fid && nfid == that.nfid
    case _ => false
  }
  override def hashCode: Int = if (nfid >= 0) mix(mix(fid.toLong, nfid.toLong), cur.toLong).toInt else mix(fid.toLong, cur.toLong).toInt
  override def toString = "feature = " + prv + "," + cur + "," + fid + "," + nfid
}

class NBinFeature(override val value: Double, prv: Int, cur: Int, fid: Int, nfid: Int = -1) extends Feature(prv, cur, fid, nfid)

/*
 * maps labels to integers
 * @param - fixed whether the alphabet is fixed or not
 * @author Ben Wellner
 */
class Alphabet[A](var fixed: Boolean) extends HashMap[A, Int] {

  def this() = this(false)
  def update(e: A): Int = get(e) match {
    case Some(v) => v
    case None => if (fixed) -1 else { update(e, size); size - 1 }
  }

  /*
   * @return - a map that takes integers and returns the corresponding label
  */
  def getInvMap = {
    val h = HashMap[Int, A]()
    (h /: this) { case (ha, (k, v)) => ha += (v -> k); ha }
  }
}

class AlphabetWithSpecialCases[A](fixed: Boolean, specialCase: (A => Boolean)) extends Alphabet[A](fixed) {
  override def update(e: A): Int =
    if (!specialCase(e)) {
      super.update(e)
    } else {
      -1
    }
}

class LongAlphabet(var fixed: Boolean) {
  def this() = this(false)
  var size = 0
  private val iMap = new OpenLongIntHashMap()
  def update(k: Long): Int = {
    val r = iMap.get(k)
    val contains = iMap.containsKey(k)
    if (contains) r
    else if (fixed) (-1)
    else {
      val s = size
      iMap.put(k, size)
      size += 1
      s
    }
  }
  def getUnderlyingMap: OpenLongIntHashMap = iMap
  def add(k: Long, v: Int) = {
    iMap.put(k, v)
    size = math.max(v, size)
  }
  def get(k: Long) = iMap.get(k)
  def update(yprv: Int, ycur: Int, fname: Long): Int = update(FeatureHashMixer.mixFeature(yprv, ycur, fname))
  def update(ycur: Int, fname: Long): Int = update(FeatureHashMixer.mixFeature(ycur, fname))
}

object FeatureHashMixer {
  import IncrementalMurmurHash.mix

  @inline
  final def mixFeature(ycur: Int, fname: Long): Long = mix(fname, ycur)

  @inline
  final def mixFeature(yprv: Int, ycur: Int, fname: Long): Long = mix(mix(fname, ycur), yprv)

  @inline
  final def mixFeature(segSize: Int, yprv: Int, ycur: Int, fname: Long): Long = mix(mix(mix(fname, ycur), yprv), segSize)

}

class RandomLongAlphabet(sz: Int) extends LongAlphabet(true) {
  import IncrementalMurmurHash._

  size = sz
  @inline
  final override def update(k: Long): Int = {
    if (k < 0) (math.abs(k) % size).toInt
    else (k % size).toInt
  }

  override def get(k: Long): Int = update(k)
  // hash this (again) before mod-ing
  def updateHash(k: Long): Int = update(mix(0L, k))
}

class SemiRandomFsetMapping(val sz: Int, val arr: Array[Set[Int]]) {
  def this(sz: Int) = this(sz,Array.fill(sz)(Set[Int]()))
  def this(ar: Array[Set[Int]]) = this(ar.size, ar)
  
  final def get(v: Long) : Set[Int] = arr((math.abs(v.toInt) % sz))
  def add(v: Long, yp: Int) = {
    val pos = math.abs(v.toInt) % sz
    val ss = arr(pos)
    arr(pos) = ss + yp
  }
  
}


/*
 * Represents a 'type' of feature that may have many specific features for different
 * label combinations.  The actual specific features associated with the feature type
 * are stored in the fdetail feature set.
 * @param fname - feature type name
 * @param edgep - whether the feature is an edge type feature (over label pairs)
 * @param segsize - feature associated with a particular segment size (for Semi-CRFs)
 * @param fcat - category for whether feature is standard, neural or multi
 * @author Ben Wellner
*/
class FeatureType(val fname: Long, val edgep: Boolean, val segsize: Int, val fcat: FeatureCat = StdFeature) extends FeatureCore {
  private var fdetailSet: collection.immutable.Set[Feature] = Set()

  def add(f: Feature) = fdetailSet += f
  def set(fs: Set[Feature]) = fdetailSet = fs

  def fdetail = fdetailSet
  def getName = fname.toString
  def value = if (fdetailSet.isEmpty) 1.0 else fdetailSet.head.value
  override def toString = getName

  override def equals(other: Any): Boolean =
    other match {
      case that: FeatureType => fname == that.fname && segsize == that.segsize && edgep == that.edgep
      case _ => false
    }
  override def hashCode: Int = 41 * (41 * (41 + fname.hashCode) + segsize) + edgep.hashCode
}

abstract class AbstractValuedFeatureType(val value: Double) extends FeatureCore {
  def getFeatures: Iterable[Feature]
  def segsize: Int
}

class ValuedFeatureType(value: Double, val ft: FeatureType) extends AbstractValuedFeatureType(value) {
  def getName = ft.getName
  def getFeatures: Iterable[Feature] =
    if (value == 1.0) ft.fdetail
    else ft.fdetail map { f => new NBinFeature(value, f.prv, f.cur, f.fid, f.nfid) }
  def segsize = ft.segsize
  def getFName = ft.fname
}

class ValuedSemiSupportedFeatureType(value: Double, val ss: Int, val fname: Long, val edgep: Boolean, 
    val faMap: LongAlphabet, fsetMap: SemiRandomFsetMapping) extends AbstractValuedFeatureType(value) {

  def segsize = ss
  def getName = fname.toString
  override def getFeatures: Iterable[Feature] = {
    val nl = CrfInstance.numLabels
    if (edgep) {
      for (i <- 0 until nl; j <- 0 until nl) yield {
        val fid = faMap.update(i, j, fname)
        new NBinFeature(value, i, j, fid, -1)
      }
    } else {
      fsetMap.get(fname) map {i =>
        val fid = faMap.update(-1, i, fname)
        new NBinFeature(value, -1, i, fid, -1)
        }
    }
  }
}

class ValuedRandomFeatureType(value: Double, val ss: Int, val fname: Long, val edgep: Boolean, val faMap: LongAlphabet) extends AbstractValuedFeatureType(value) {
  def segsize = ss
  def getName = fname.toString
  // this will let us "lazily" compute features
  override def getFeatures: Iterable[Feature] = {
    val nl = CrfInstance.numLabels
    if (edgep) {
      for (i <- 0 until nl; j <- 0 until nl) yield {
        new NBinFeature(value, i, j, faMap.update(i, j, fname), -1)
      }
    } else {
      for (i <- 0 until nl) yield {
        new NBinFeature(value, -1, i, faMap.update(-1, i, fname), -1)
      }
    }
  }
}

case class PreFeature(val prv: Int, val cur: Int, val name: Long)

abstract class FeatureRep[Obs](val semiCrf: Boolean) {
  def createSource(l: Int, o: Obs, b: Boolean, i: Option[Map[String, String]]): ObsSource[Obs]
  def createSource(l: Int, o: Obs, b: Boolean): ObsSource[Obs]
  // these two cases are for recoding
  def createSource(l: Int, o: Obs, b: Boolean, i: Option[Map[String, String]], st: Int, en: Int) = new RecodedObsSource(l, o, b, i, st, en)
  def createSource(l: Int, o: Obs, b: Boolean, st: Int, en: Int) = new RecodedObsSource(l, o, b, None, st, en)

  def createInstance(l: Int, o: Int, sId: Int): AbstractInstance

  def createInstance(l: Int, o: Int): AbstractInstance

  var maxSegSize = 0 // zero based
  def getFeatureSetName: String
  def getLexicon: Option[BloomLexicon]
  def getWordProps: Option[WordProperties]
  def getWordScores: Option[WordScores]
  def getInducedFeatureMap: Option[InducedFeatureMap]

  var otherIndex = -1 // keep track of index for "other" label associated with sparse labeling tasks
}

/*
 * The <code>FeatureRep</code> class provides functionality for creating <code>ObsSource<code> objects
 * and them mapping them instances.  The default behavior is to create <code>CrfInstance<code> 
 * and subclasses will need to override appropriately to create other instance types.
 * @param maxSegSize - integer indicating the maximum segment size for Semi-CRFs
 * @author Ben Wellner
*/
abstract class FactoredFeatureRep[Obs](semi: Boolean) extends FeatureRep[Obs](semi) {

  val mgr: FeatureManager[Obs]

  def createSource(l: Int, o: Obs, b: Boolean, i: Option[Map[String, String]]): ObsSource[Obs] = {
    val src = new ObsSource(l, o, b, i)
    mgr.lex match {
      case Some(lex) =>
        src.setLexCodes(lex.get(o.toString))
      case None =>
    }
    src
  }

  def createSource(l: Int, o: Obs, b: Boolean): ObsSource[Obs] = new ObsSource(l, o, b, None)
  def createInstance(l: Int, o: Int, sId: Int) = new CrfInstance(l, o, sId)
  def createInstance(l: Int, o: Int) = new CrfInstance(l, o, -1)

  def getFeatureSetName = mgr.iString
  def getLexicon = mgr.lex
  def getWordProps = mgr.wdProps
  def getWordScores = mgr.wdScores
  def getInducedFeatureMap = mgr.inducedFeatureMap

  val fsetMap: OpenLongObjectHashMap

  /*
   * Takes a single instance, <i>inst</i>, the entire sequence of Sources, <i>dseq</i>
   * and the current position in <i>dseq</i> that corresponds to the provided instance, 
   * <i>inst</i>. It applies the features specified in the <code>FeatureManager</code> object
   * to the current position and stores the resulting features in <i>inst</i>
   * @param inst - instance
   * @param dseq - entires SourceSequence
   * @param pos - position within the sequence of <i>inst</i>
   * @return <code>Unit</code> 
  */
  def applyFeatureFns(inst: CrfInstance, dseq: SourceSequence[Obs], pos: Int, static: Boolean): Unit

  protected def addFeatureStatic(ss: Int, inst: CrfInstance, fname: BuiltFeature) =
    fsetMap.get(fname.get) match {
      case (ft: FeatureType) =>
        inst add (new ValuedFeatureType(fname.value, ft))
      case _ =>
    }

  protected def addFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: Long, vl: Double, supporting: Boolean, fcat: FeatureCat): Unit
  protected def addFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: BuiltFeature, supporting: Boolean = false, fcat: FeatureCat = StdFeature): Unit = {
    addFeature(ss, inst, yprv, yp, fname.get, fname.value, supporting, fcat)
  }

  val displacedMap = new collection.mutable.HashMap[Obs, FeatureReturn]
  val disInt = IncrementalMurmurHash.hash("DIS", 0)

  def resetDisplacement: Unit = displacedMap.clear

  protected def updateDisplaceableFeatures(dseq: SourceSequence[Obs], pos: Int, fresult: FeatureReturn) = {
    if (fresult.features.length > 0) {
      val obs = dseq(pos).obs
      val r = displacedMap.get(obs) match { case Some(r) => r case None => new FeatureReturn }
      val nr = r.join(fresult)
      nr.displaced_=(true)
      displacedMap.put(obs, fresult)
    }
  }

  protected def addDisplacedFeatures(inst: CrfInstance, d: Int, dseq: SourceSequence[Obs], pos: Int, yp: Int, yprv: Int, static: Boolean) = {
    val obs = dseq(pos).obs
    displacedMap.get(obs) match {
      case Some(fr) =>
        fr.update(disInt) // make features displaced
        fr.features foreach { f =>
          if (static) addFeatureStatic(d, inst, f) else addFeature(d, inst, (if (fr.edgeP) yprv else (-1)), yp, f, false, fr.fcat)
        }
      case None =>
    }
  }

}

/*
 * A decoding feature representation. Features are added differently at decoding time in
 * the sense that only features that appared at training time are added and there isn't 
 * a need to keep track of features.
 * @param opts - Command-line specified options that control decoder behavior
 * @param model - The trained model used for decoding
 * @param maxSegSize - passed to super constructor
 * @author Ben Wellner
*/
class DecodingFactoredFeatureRep[Obs](val mgr: FeatureManager[Obs], opts: Options, model: StdModel, preDecoder: Boolean = false) extends FactoredFeatureRep[Obs]((model.segSize > 1)) {

  def this(opts: Options, m: StdModel, pre: Boolean = false) = this(FeatureManager[Obs](opts, m, pre), opts, m)

  var randomModel = (model.isInstanceOf[RandomStdModel])
  CrfInstance.randomFeatures = randomModel
  CrfInstance.numLabels = model.getLabelAlphabet.size
  val fsetMap: OpenLongObjectHashMap = model.fsetMap
  val faMap: LongAlphabet = {
    model.deriveFaMap
  }
  val semiRandomFset = model match {case m: RandomStdModel => m.randFsetMap case _: StdModel => new SemiRandomFsetMapping(1)}
  val useSemiRandomFeatures = semiRandomFset.sz > 10
  
  mgr.lex_=(if (mgr.lex.isEmpty) model.lex else mgr.lex)
  mgr.wdProps_=(if (mgr.wdProps.isEmpty) model.wdProps else mgr.wdProps)
  mgr.wdScores_=(if (mgr.wdScores.isEmpty) model.wdScores else mgr.wdScores)
  mgr.inducedFeatureMap_=(model.inducedFs match {
    case Some(m) =>
      println("got induced feature set: " + m.hmap.get.size); Some(m)
    case None => InducedFeatureMap()
  })
  maxSegSize_=(model.segSize - 1)

  protected def addFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: Long, vl: Double, supporting: Boolean, fcat: FeatureCat): Unit =
    addFeature(ss, inst, fname, vl, false, false)

  def addFeature(ss: Int, inst: CrfInstance, fname: BuiltFeature, self: Boolean, edgeP: Boolean): Unit =
    addFeature(ss, inst, fname.get, fname.value, self, edgeP)

  def addFeature(ss: Int, inst: CrfInstance, fname: Long, vl: Double, self: Boolean, edgeP: Boolean): Unit = {
    if (randomModel && !useSemiRandomFeatures) {
      addRandomFeature(ss, inst, fname, vl, edgeP)
    } else if (randomModel && useSemiRandomFeatures) { // check for semirandom supported feature mode here
      addSemiRandomSupportedFeature(ss, inst, fname, vl, edgeP)
    } else {
      fsetMap.get(fname) match {
        case (ft: FeatureType) =>
          inst add (new ValuedFeatureType(vl, ft))
        case _ =>
      }
    }
  }
  
  private def addSemiRandomSupportedFeature(ss: Int, inst: CrfInstance, fname: Long, vl: Double, edgeP: Boolean): Unit = {
    val nl = CrfInstance.numLabels
    if (edgeP) {
      var i = 0
      var j = 0
      while (i < nl) {
        j = 0
        while (j < nl) {
          inst add new NBinFeature(vl, i, j, faMap.update(i,j,fname), -1)
          j += 1
        }
        i += 1
      }      
    } else {
      semiRandomFset.get(fname) foreach {i =>
        inst add new NBinFeature(vl, -1, i, faMap.update(-1, i, fname), -1)
        }
    }
  }

  @inline
  private def addRandomFeature(ss: Int, inst: CrfInstance, fname: Long, vl: Double, edgeP: Boolean): Unit = {
      val nl = CrfInstance.numLabels
      if (edgeP) {
        var i = 0
        var j = 0
        while (i < nl) {
          j = 0
          while (j < nl) {
            inst add new NBinFeature(vl, j, i, faMap.update(j,i,fname))
            j += 1
          }
          i += 1
        }
      } else {
        var i = 0
        while (i < nl) {
          inst add new NBinFeature(vl, -1, i, faMap.update(-1,i,fname))
          i += 1
        }
      }
  }

  def applyFeatureFns(inst: CrfInstance, dseq: SourceSequence[Obs], pos: Int, static: Boolean = false): Unit = {
    val upTo = math.min(maxSegSize, pos)
    addDisplacedFeatures(inst, 0, dseq, pos, -1, -1, true)
    mgr.fnList foreach { fn =>
      if (upTo > 0) {
        for (d <- 0 to upTo) {
          val fresult: FeatureReturn = fn(d, dseq, pos)
          if (!fresult.edgeP || (pos > 0)) {
            fresult.features foreach { f => addFeature(d, inst, f, fresult.self, fresult.edgeP) }
            if (fresult.displaced) updateDisplaceableFeatures(dseq, pos, fresult)
          }
        }
      } else {
        val fresult: FeatureReturn = fn(0, dseq, pos)
        if (!fresult.edgeP || (pos > 0)) 
          fresult.features foreach { f =>
            addFeature(0, inst, f, fresult.self, fresult.edgeP) }
        if (fresult.displaced) updateDisplaceableFeatures(dseq, pos, fresult)
      }
    }
  }
}

class SelfInducibleDecodingFactoredFeatureRep[Obs](mgr: FeatureManager[Obs], opts: Options, model: StdModel)
  extends DecodingFactoredFeatureRep[Obs](mgr, opts, model) {
  def this(opts: Options, m: StdModel) = this(FeatureManager.createForSelfTraining[Obs](opts, m), opts, m)

  override def createInstance(l: Int, o: Int, sId: Int) = new SelfInducibleCrfInstance(l, o, sId)
  override def createInstance(l: Int, o: Int) = new SelfInducibleCrfInstance(l, o, -1)

  override def addFeature(ss: Int, inst: CrfInstance, fname: Long, vl: Double, self: Boolean, edgeP: Boolean): Unit = {
    super.addFeature(ss, inst, fname, vl, self, edgeP)
    if (self) inst.addSelf(fname) // add this feature (regardless of whether we saw it during training) to self-inducible feature set
  }
}

/*
* A training feature representation. Parallels the one for decoding but requires additional structure to keep
 * track of newly constructed features (for saving those in the generated model) as well as maps
 * for features and feature types.
 * @param opts - Command-line training options
 * @param supporting - If true, this run won't actually add features but just modify the maps/symbol tables keeping track of features
 * @param maxSegSize - Passed to super constructor
 * @author Ben Wellner
*/
class TrainingFactoredFeatureRep[Obs](val mgr: FeatureManager[Obs], opts: Options, var supporting: Boolean, semi: Boolean)
  extends FactoredFeatureRep[Obs](semi) {

  def this(opts: Options, supporting: Boolean, semi: Boolean) = this(FeatureManager[Obs](opts), opts, supporting, semi)
  def this(mgr: FeatureManager[Obs], opts: Options) = this(mgr, opts, false, opts.semiCrf)
  def this(opts: Options) = this(opts, false, opts.semiCrf)
  
  val random = opts.randomFeatures || opts.randomSupportedFeatures
  
  var featureTypeSet : Set[Long] = Set() // keep track of all feature types to estimate # of random features dynamically
  var numFeatureTypes = -1

  lazy val numSemiRandomFeatureTypes = PrimeNumbers.getLargerPrime((numFeatureTypes * opts.randomSupportedCoefficient).toInt)
  lazy val numRandomFeatures = 
    PrimeNumbers.getLargerPrime((numFeatureTypes * CrfInstance.numLabels * opts.randomFeatureCoefficient).toInt)
         
  lazy val semiRandomFset = new SemiRandomFsetMapping(if (opts.randomSupportedFeatures) numSemiRandomFeatureTypes else 0)
  
  val initialModel = opts.initialModel match { case Some(mfile) => Some(StandardSerializer.readModel(mfile)) case None => None }
  lazy val faMap: LongAlphabet = initialModel match {
    case Some(m) => m.deriveFaMap
    case None =>
      if (random) {
        new RandomLongAlphabet(numRandomFeatures)
      } else new LongAlphabet
  }

  lazy val neuralFaMap: LongAlphabet =
    if (random) {
      new RandomLongAlphabet(numRandomFeatures)
    } else new LongAlphabet

  val fsetMap: OpenLongObjectHashMap = initialModel match { case Some(m) => m.fsetMap case None => new OpenLongObjectHashMap() }

  protected def addRandFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: Long, vl: Double): Unit = {
    //fsetFilterBuilder.add(fname)
    if (yprv == (-2))
      inst add new ValuedRandomFeatureType(vl, ss, fname, false, faMap)
    else {
      inst add new ValuedRandomFeatureType(vl, ss, fname, true, faMap)
    }
  }
  
  protected def addSemiRandomSupportedFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: Long, vl: Double): Unit = {
    if (yprv == (-2)) {
      semiRandomFset.add(fname, yp) // update this for non-edge features
      inst add new ValuedSemiSupportedFeatureType(vl, ss, fname, false, faMap, semiRandomFset)
    } else {
      inst add new ValuedSemiSupportedFeatureType(vl, ss, fname, true, faMap, semiRandomFset)
    }
    
  }

  protected def addFeature(ss: Int, inst: CrfInstance, yprv: Int, yp: Int, fname: Long, vl: Double, supporting: Boolean, fcat: FeatureCat): Unit = {
    if (opts.randomFeatures)
      addRandFeature(ss, inst, yprv, yp, fname, vl)
    else if (opts.randomSupportedFeatures) {
      addSemiRandomSupportedFeature(ss, inst, yprv, yp, fname, vl)
    } else {
      val ft =
        fsetMap.get(fname) match {
          case v: FeatureType => v
          case _ =>
            val n = new FeatureType(fname, (yprv >= 0), ss, fcat)
            fsetMap.put(fname, n)
            n
        }
      if (yp >= 0) {
        val fid = ft.fcat match { case NNFeature => -1 case _ => faMap.update(yprv, yp, fname) }
        val nfid = ft.fcat match { case StdFeature => -1 case _ => neuralFaMap.update(yp, fname) }
        ft add new Feature(yprv, yp, fid, nfid)
      }
      if (!supporting) inst add new ValuedFeatureType(vl, ft)
    }
  }

  private def getEffectiveSize(yp: Int, cp: Int, dseq: SourceSequence[Obs]): Int = {
    val sz =
      if ((yp != otherIndex) && (cp < (dseq.length - 1)) && !dseq(cp + 1).beg) -1
      else if (cp == 0) 0
      else {
        var d = 0
        var continue = true
        //while ((cp - d) >= 0 && d < maxSegSize && continue) {
        while ((cp - d) >= 0 && continue) {
          if (dseq(cp - d).beg) continue = false
          d += 1
        }
        d - 1
      }
    if (sz > maxSegSize) {
      maxSegSize = sz
      CrfInstance.maxSegSize = sz
    }
    sz
  }

  def extractSupportedFeatures(inst: CrfInstance, dseq: SourceSequence[Obs], pos: Int): Unit = {
    val yp = dseq(pos).label
    val d = if (semiCrf) getEffectiveSize(yp, pos, dseq) else 0
    // only add features that correspond to segments that appear in the training data
    if (d >= 0) {
      val yprv = if (pos - d > 0) dseq(pos - d - 1).label else (-1)
      addDisplacedFeatures(inst, d, dseq, pos, yp, yprv, false)
      mgr.fnList foreach { fn =>
        val freturn: FeatureReturn = fn(d, dseq, pos)
        if (!freturn.edgeP || (pos > 0)) {
          freturn.features foreach { f => addFeature(d, inst, (if (freturn.edgeP) yprv else (-1)), yp, f, true, freturn.fcat) }
          if (freturn.displaced) updateDisplaceableFeatures(dseq, pos, freturn)
        }
      }
    }
  }
  

  def countFeatureTypes(dseq: SourceSequence[Obs], pos: Int) = {
    val upTo = (maxSegSize min pos)
    val yp = dseq(pos).label
    for (d <- 0 to upTo) {
      val yprv = if (pos - d > 0) dseq(pos - d - 1).label else (-1)
      mgr.fnList foreach { fn =>
        val fresult: FeatureReturn = fn(d, dseq, pos)
        if (!fresult.edgeP || (pos > 0)) {
          fresult.features foreach { f =>
            featureTypeSet += f.get
          }
        }
      }
    }
  }
  // For handling learning with unlabeled elements we need to do:
  //   Set this up so that if the label is -1, we add in all possible labels as if it were an unsupported feature type
  def applyFeatureFns(inst: CrfInstance, dseq: SourceSequence[Obs], pos: Int, static: Boolean = false): Unit = {
    val upTo = (maxSegSize min pos)
    val yp = dseq(pos).label
    for (d <- 0 to upTo) {
      val yprv = if (pos - d > 0) dseq(pos - d - 1).label else (-1)
      addDisplacedFeatures(inst, d, dseq, pos, yp, yprv, static)
      mgr.fnList foreach { fn =>
        val fresult: FeatureReturn = fn(d, dseq, pos)
        if (!fresult.edgeP || (pos > 0)) {
          fresult.features foreach { f =>
            if (static) addFeatureStatic(d, inst, f) else addFeature(d, inst, (if (fresult.edgeP) yprv else (-2)), yp, f, false, fresult.fcat)
          }
          if (fresult.displaced) updateDisplaceableFeatures(dseq, pos, fresult)
        }
      }
    }
  }
}

class NonFactoredPreFeature(val unsupported: Boolean, val yprv: Int, val ycur: Int, val fname: Long, val value: Option[Double]) {
  def this(u: Boolean, yprv: Int, ycur: Int, fname: Long) = this(u, yprv, ycur, fname, None)
  def this(ycur: Int, fname: Long) = this(false, -1, ycur, fname, None)

  def conjoin(that: NonFactoredPreFeature) = {
    assert(yprv == that.yprv)
    assert(ycur == that.ycur)
    new NonFactoredPreFeature(unsupported, yprv, ycur, that.fname + fname)
  }
}

/*
 * Feature Representation for non-Factored features. Non-factored models are simpler in that 
 * thare is a one-to-one correspondance between a "feature type" and a "feature". 
*/
class NonFactoredFeatureRep[Obs](val opts: Options, val mgr: NonFactoredFeatureManager[Obs], val supporting: Boolean, val maxLab: Int, wholeSeq: Boolean) extends FeatureRep[Obs](false) {

  def this(mgr: NonFactoredFeatureManager[Obs], s: Boolean, ml: Int, ws: Boolean) = this(new Options, mgr, s, ml, true)
  def this(mgr: NonFactoredFeatureManager[Obs], ml: Int) = this(mgr, false, ml, false)

  val random = opts.numRandomFeatures > 0
  def createSource(l: Int, o: Obs, b: Boolean, i: Option[Map[String, String]]): ObsSource[Obs] = new ObsSource((l min maxLab), o, b, i)
  def createSource(l: Int, o: Obs, b: Boolean): ObsSource[Obs] = new ObsSource((l min maxLab), o, b, None)
  def getFeatureSetName = mgr.iString
  def getLexicon = mgr.lex
  def getWordProps = mgr.wdProps
  def getWordScores = None
  def getInducedFeatureMap = None

  def createInstance(l: Int, o: Int, sId: Int): NonFactoredCrfInstance = if (random) new FastNonFactoredCrfInstance(l, o) else new NonFactoredCrfInstance(l, o, sId)
  def createInstance(l: Int, o: Int): NonFactoredCrfInstance = if (random) new FastNonFactoredCrfInstance(l, o) else new NonFactoredCrfInstance(l, o, -1)

  val faMap: LongAlphabet =
    if (opts.numRandomFeatures > 0) {
      val nf = if (opts.numRandomFeatures > 10) opts.numRandomFeatures else 115911564
      new RandomLongAlphabet(nf)
    } else new LongAlphabet

  def convertPreFeatures(inst: NonFactoredCrfInstance, res: FeatureReturn, ycur: Int, yprv: Int) =
    res.features foreach { fea =>
      val f = faMap update fea.hv
      if (f >= 0) inst add (new NBinFeature(fea.value, yprv, ycur, f))
    }

  def gatherFeatures(dseq: SourceSequence[Obs], pos: Int): Unit = {
    val ycur = dseq(pos).label
    if (mgr.fineFnList.length > 0) {
      mgr.fineFnList foreach { fn =>
        val res = fn(1, dseq, pos, ycur)
        res.features foreach { fea =>
          faMap update fea.hv
        }
      }
    }
  }

  def applyFeatureFns(sup: Option[(Int, Int)], inst: NonFactoredCrfInstance, dseq: SourceSequence[Obs], pos: Int): Unit = {
    var i = 0
    val effectiveMax = if (wholeSeq) dseq.length else math.min(maxLab, pos)
    val ycur = dseq(pos).label
    if (mgr.fineFnList.length > 0) {
      while (i < effectiveMax) {
        mgr.fineFnList foreach { fn =>
          val res = fn(1, dseq, pos, i)
          convertPreFeatures(inst, res, i, -1)
        }
        i += 1
      }
    }
    // this doesn't make sense to have - only "fine" features extraction fns are allowed (for now)
    //mgr.fnList foreach { fn => convertPreFeatures(inst, fn(1,dseq,pos), ycur, yprv)}
  }
}

class DecodingNonFactoredFeatureRep[Obs](fm: LongAlphabet, mgr: NonFactoredFeatureManager[Obs], maxLab: Int, ws: Boolean) extends NonFactoredFeatureRep[Obs](mgr, false, maxLab, ws) {
  def this(fm: LongAlphabet, mgr: NonFactoredFeatureManager[Obs], ml: Int) = this(fm, mgr, ml, false)
  override val faMap: LongAlphabet = { fm.fixed_=(true); fm }
  override val random = true
}



