/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

/*
 * Top-level abstract class for representing labeled elements in a sequence
 * @param label - the integer label for this sequence element
*/
abstract class SeqElement(var label: Int) {
  def getRange: Int
  def conditionalProb(i: Int): Double
  def setConditionalProb(i: Int, v: Double): Unit
  def getCondProbTable : Map[Int,Double]
}

/*
 * Represents an instance with an associated set of features for use by decoders
 * or trainers
 * @param lab - integer label.  By convention, -1 denotes UNCERTAINTY in label
 * @param orig - original label (preserved for scoring and other reasons) 
 * @param segId - an integer, i, indicating its the ith segment in a sequence
 * @author Ben Wellner
*/
abstract class AbstractInstance(label: Int, val orig: Int, var segId: Int) extends SeqElement(label) {
  type FType <: FeatureCore

  var userFeatures: Set[FType] = Set() // ( we create a lot of these objects )
  var normalized = false
  var condProbTbl: Map[Int, Double] = Map()
  def getCompactVec: Array[CompactFeature]
  def getRange = condProbTbl.size // used when # of states/labels isn't stationary over data points
  def conditionalProb(i: Int): Double = {
    ensureNormalized
    condProbTbl.get(i) match { case Some(v) => v case None => 1.0 }
  }
  def setConditionalProb(i: Int, v: Double): Unit = condProbTbl += (i -> v)
  def add(ft: FType): Unit = {
    userFeatures += ft
  }
  
  def getCondProbTable = condProbTbl
  
  @inline
  private def ensureNormalized = {
    if (!normalized) {
      var s = 0.0
      condProbTbl foreach {case (i,v) => s += v}
      var i = 0; while (i < getRange) {
        condProbTbl += (i -> condProbTbl(i) / s)
        i += 1
      }
      normalized = true
    }
  }
  def hasPosterior = condProbTbl.size > 0
  def userVec: Set[FType] = userFeatures
  def getCompVec: Array[Array[Feature]]
  def addSelf(l: Long) = {}
  def selfUserVec: Set[Long] = Set()
  lazy val instWeight : Double = 1.0
  
}

class CrfInstance(label: Int, orig: Int, segId: Int, cv: Option[Array[Array[Feature]]] = None) extends AbstractInstance(label, orig, segId) {

  type FType = AbstractValuedFeatureType

  private val seg1Features = new ArrayBuffer[Feature]
  private var compVec: Option[Array[Array[Feature]]] = cv
  var dimension1 = -1
  def getCompactVec = throw new RuntimeException("Unimplemented")
  def getCompVec: Array[Array[Feature]] =
    if (CrfInstance.useCache) {
      if ((!CrfInstance.training && CrfInstance.maxSegSize < 1))
        Array(seg1Features.toArray) // case where we're decodign with standard CRFs
      else
        getCacheCompVec // other cases we compile out feature vec on the fly
    } else getNoCacheCompVec
  private def getNoCacheCompVec: Array[Array[Feature]] = compVec match { case None => expandVector case Some(cv) => cv }
  private def getCacheCompVec: Array[Array[Feature]] = compVec match {
    case None =>
      val cv = expandVector
      compVec = Some(cv)
      cv
    case Some(cv) => cv
  }

  // this speeds things up a bit for non-Semi CRF
  override def add(ft: FType) : Unit =
    if (CrfInstance.useCache && (CrfInstance.maxSegSize < 1) && !CrfInstance.training) {
      ft.getFeatures foreach { fc => seg1Features += fc }
    } else {
      super.add(ft) // semi-CRF case and/or for training
    }
  
  def add(f: Feature) = {
    seg1Features += f // directly add a feature
  }

  private def expandVector: Array[Array[Feature]] = {
    val fs = Array.tabulate(CrfInstance.maxSegSize + 1) { _ => new ArrayBuffer[Feature] }
    userVec foreach { ft => ft.getFeatures foreach { e => fs(ft.segsize) append e } }
    fs map (_.toArray)
  }
}

/*
 * Represents an instance with an associated set of features for use by decoders
 * or trainers. Instances are compiled out with no mechanism for lazy evaluation.
 * This has advantages when instances must be serialized as it avoids excessive 
 * (global) state.
 * @param lab - integer label.  By convention, -1 denotes UNCERTAINTY in label
 * @param orig - original label (preserved for scoring and other reasons) 
 * @param segId - an integer, i, indicating its the ith segment in a sequence
 * @author Ben Wellner
*/
class CompiledCrfInstance(label: Int, orig: Int, segId: Int, val cv: Array[Array[Feature]]) extends AbstractInstance(label, orig, segId) {

  type FType = AbstractValuedFeatureType
  def getCompVec = cv
  def getCompactVec = throw new RuntimeException("Unimplemented")  

}

object CompiledCrfInstance {
  def apply(instance: AbstractInstance) : CompiledCrfInstance = {
    val cv = instance.getCompVec
    new CompiledCrfInstance(instance.label, instance.orig, instance.segId, cv)
  }
}

class SelfInducibleCrfInstance(label: Int, orig: Int, segId: Int, cv: Option[Array[Array[Feature]]] = None) extends CrfInstance(label, orig, segId, cv) {
  var selfUserFeatures: Set[Long] = Set()

  override def addSelf(l: Long): Unit = selfUserFeatures += l
  override def selfUserVec: Set[Long] = selfUserFeatures
}

object CrfInstance {
  var useCache = true
  var training = false
  var maxSegSize = 0 // this is "0" based - so 0 means segment size 1 observation, 1 is size 2, etc.
  var diskCache: Option[String] = None
  var randomFeatures = false
  var numLabels = 0 // global for number of labels
}

/*
 * A sequence classification instance for  non-factored models/features.  These instances don't
 * have FeatureTypes
 * @param label - integer label
 * @param orig - original label
 * @param segid - segment id
 * @author Ben Wellner
*/
class NonFactoredCrfInstance(label: Int, orig: Int, segId: Int, cv: Option[Array[Array[Feature]]] = None) extends AbstractInstance(label, orig, segId) {
  type FType = Feature
  private var compVec: Option[Array[Array[Feature]]] = cv

  def getCompactVec = throw new RuntimeException("Unsupported")
  def getCompVec: Array[Array[Feature]] = compVec match {
    case Some(v) => v
    case None =>
      val fs = new ArrayBuffer[Feature]
      userVec foreach { fs += _ }
      Array(fs.toArray)
  }
}

class FastNonFactoredCrfInstance(label: Int, orig: Int) extends NonFactoredCrfInstance(label,orig,-1,None) {

  private val fVec = new ArrayBuffer[Feature]
  
  override def add(ft: FType) = fVec += ft
  override def getCompVec : Array[Array[Feature]] = Array(fVec.toArray)
}

/*
 * An ObsSource is also associated with a particular position in a sequence and has
 * a label.  It is generated before an AbstractInstance <i>before</i> feature extraction
 * has occurred. The result of feature extraction applied to a sequence of Source objects
 * is a sequence of AbstractInstances
 * @param lab - label
 * @param obs - an observation (e.g. a word)
 * @param beg - whether this is the beginning of a segment
 * @param info - optional additional information (attribute value pairs) associated with this Source
*/
class ObsSource[Obs](lab: Int, val obs: Obs, val beg: Boolean, var info: Option[Map[String, String]], var condTable: Map[Int,Double] = Map()) extends SeqElement(lab) {
  def this (obs: Obs, beg: Boolean, info: Option[Map[String,String]], ct: Map[Int,Double]) = this(0,obs,beg,info,ct)
  import IncrementalMurmurHash._
  lazy val st = obs.toString
  //val code = if (isNumberString(st)) numSpecialHash else hash(st)
  lazy val code = hash(st)
  lazy val prefCode = 
    if (st.size < 6) code
    else if (isNumberString(st)) numSpecialHash
    else hash(st.substring(0, 5))
  var lexCodes: List[Long] = Nil // to optimize this
  lazy val posCode = info match { case Some(map) => map.get("pos") match { case Some(v) => hash(v) case None => 0L } case None => 0L }
  // infoCodes will contain hashcodes for auxilliary token-level attributes (specified via lex/token encoded attribute value pairs)
  // in some cases st and en offsets are included in this map; we don't want to generate codes for these
  lazy val infoCodes = info map { m => ((m.filter{case (k,_) => k != "st" && k != "en"}) map { case (k, v) => mix(hash(v), hash(k))}).toArray }
  lazy val cPosCode = info match { case Some(map) => map.get("pos") match { case Some(v) => hash(v(0).toString) case None => 0L } case None => 0L }
  var preLabelCode = 0L
  def getRange = -1 // used when # of states/labels isn't stationary over data points
  def start = -1
  def end = -1
  def conditionalProb(i: Int) : Double = condTable.get(i).getOrElse(1.0)
  def setConditionalProb(i: Int, v: Double) = condTable += (i -> v)
  def getCondProbTable = condTable

  def addInfo(a: String, v: String) = info match {
    case Some(m) => info = Some(m + (a -> v))
    case None => info = Some(Map(a -> v))
  }

  def setLexCodes(l: Option[List[Long]]) = l match { case Some(l) => lexCodes = l case None => }
}


/*
 * A recoded ObsSource keeps track of where it is situatated in the original ObsSource sequence
 * with start and end indices
 * @param lab - label
 * @param obs - an observation (e.g. a word)
 * @param beg - whether this is the beginning of a segment
 * @param info - optional additional information (attribute value pairs) associated with this ObsSource
 * @param start - start index in original sequence
 * @param end - end index in original sequence
*/
class RecodedObsSource[Obs](lab: Int, obs: Obs, beg: Boolean, info: Option[Map[String, String]],
  override val start: Int, override val end: Int) extends ObsSource[Obs](lab, obs, beg, info) {
  def this(lab: Int, obs: Obs, beg: Boolean, st: Int, en: Int) = this(lab, obs, beg, None, st, en)
} 
