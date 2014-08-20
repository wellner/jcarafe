/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import org.mitre.jcarafe.util._

abstract class FeatureCat
case object NNFeature extends FeatureCat
case object MultiFeature extends FeatureCat
case object StdFeature extends FeatureCat

class BuiltFeature(var value: Double) extends Serializable {
  import IncrementalMurmurHash._
  def this() = this(1.0)
  var hv: Long = 0L // feature id hash value, built incrementally

  def @@(l: Char) = {
    hv = mix(hv, l.toByte)
    this
  }

  def @@(i: Long) = {
    hv = mix(hv, i)
    this
  }

  def @@(l: String) = {
    hv = hash(l, hv)
    this
  }

  def @@@(l: Seq[String]) = {
    l foreach { s => hv = hash(s, hv) }
    this
  }

  def @@@(l: Array[Byte]) = {
    hv = hash(l, l.length, hv)
    this
  }

  def iseq(l: Seq[Int]) = {
    l foreach { s => hv = mix(hv, s) }
    this
  }

  def iseqI(l: Seq[Int], i: Long) = {
    l foreach { s => hv = mix(hv, s) }
    hv = mix(hv, i)
    this
  }

  def @@@(bf: BuiltFeature) = {
    hv = mix(hv, bf.hv)
    value = this.value * bf.value
    this
  }

  def get = hv
  override def toString = get.toString
}

class BuiltFeatureDebug(vl: Double) extends BuiltFeature(vl) {
  def this() = this(1.0)
  import IncrementalMurmurHash._
  val sb = new StringBuilder

  override def @@(l: Char) = {
    sb += l
    super.@@(l)
  }

  override def @@(i: Long) = {
    sb append i.toString
    super.@@(i)
  }

  override def @@(l: String) = {
    sb append l
    super.@@(l)
  }

  override def @@@(l: Seq[String]) = {
    l foreach { sb append _ }
    super.@@@(l)
  }

  override def @@@(l: Array[Byte]) = {
    sb append (new String(l))
    super.@@@(l)
  }

  override def iseq(l: Seq[Int]) = {
    l foreach { i => sb append ("_" + i) }
    super.iseq(l)
  }

  override def iseqI(l: Seq[Int], i: Long) = {
    l foreach { i => sb append ("_" + i) }
    sb append ("_" + i)
    super.iseqI(l, i)
  }

  def @@@(bf: BuiltFeatureDebug) = {
    sb append bf.sb
    super.@@@(bf)
  }

  override def toString = sb.toString
}

object BuiltFeature extends Serializable {
  var debug = false

  def getBf(): BuiltFeature = if (debug) new BuiltFeatureDebug() else new BuiltFeature()

  def apply(): BuiltFeature = getBf()

  // this essentially does a copy
  def apply(bf: BuiltFeature): BuiltFeature = {
    apply(bf.get, bf.value)
  }

  def apply(s: String): BuiltFeature = {
    val bf = getBf()
    bf @@ s
    bf
  }

  def apply(ar: Array[Byte]): BuiltFeature = {
    val bf = getBf()
    bf @@@ ar
    bf
  }

  def apply(s: String, v: Double): BuiltFeature = {
    val bf = if (debug) new BuiltFeatureDebug(v) else new BuiltFeature(v)
    bf @@ s
    bf
  }

  def apply(i: Long): BuiltFeature = apply(i, 1.0)

  def apply(i: Long, v: Double): BuiltFeature = {
    val bf = new BuiltFeature(v)
    bf @@ i
    bf
  }

  def apply(sl: Seq[String], v: Double = 1.0): BuiltFeature = {
    val bf = new BuiltFeature(v)
    bf @@@ sl
    bf
  }

}

/**
 * A list of pairs of feature names along with their value that have fired
 * based upon the application of a feature function <code>FeatureFn</code>.
 * Specifies whether the features are <i>node</i> or <i>edge</i> features.
 * @param pairs   List of pairs
 * @param edgeP   <code>Boolean</code> indicating <i>edge</i> features when true and <i>node</i> otherwise
 */
class FeatureReturn(val features: List[BuiltFeature], var edgeP: Boolean, val self: Boolean = false,
  var fcat: FeatureCat = StdFeature, var displaced: Boolean = false) extends Serializable {

  def this(b: BuiltFeature, ep: Boolean) = this(List(b), ep)
  def this(b: BuiltFeature) = this(b, false)
  def this(v: Long) = this(BuiltFeature(v), false)
  def this(f: Long, v: Double) = this(BuiltFeature(f, v), false)
  def this(s: String) = this(BuiltFeature(s), false)
  def this(s: String, e: Boolean) = this(BuiltFeature(s), e)
  def this(s: String, v: Double) = this(BuiltFeature(s, v), false)
  def this(s: String, v: Double, e: Boolean) = this(BuiltFeature(s, v), e)
  def this(ar: Array[Byte]) = this(BuiltFeature(ar), false)
  def this(ss: List[String]) = this(ss map { BuiltFeature(_) }, false)
  def this(ss: List[String], v: Double) = this(BuiltFeature(ss, v), false)
  def this() = this(Nil, false)
  def updateS(ns: String) = { features foreach { _ @@ ns }; this }
  def updateS(nss: Seq[String]) = { features foreach { _ @@@ nss }; this }
  def update(ns: Long) = { features foreach { _ @@ ns }; this }
  def update(nss: Array[Byte]) = { features foreach { _ @@@ nss }; this }
  def update(nss: Array[Byte], i: Int) = { features foreach { _ @@@ nss @@ i }; this }
  def update2(i1: Long, i2: Long) = { features foreach { bf => bf @@ i1 @@ i2 }; this }
  def updateSI(nss: Seq[Int]) = { features foreach { _ iseq nss }; this }
  def updateSI(nss: Seq[Int], i: Int) = { features foreach { _ iseqI (nss, i) }; this }
  def join(that: FeatureReturn) = new FeatureReturn(this.features reverse_::: that.features, (this.edgeP && that.edgeP), (this.self || that.self))
  def join(other: BuiltFeature) = new FeatureReturn(other :: this.features, this.edgeP, this.self)
  def setAsEdge() = edgeP = true
  def setAsNeural() = fcat = NNFeature
  def setAsMulti() = fcat = MultiFeature
  override def toString = features.foldLeft("")(_ + " " + _)
}

object FeatureReturn extends Serializable {
  def apply(ss: List[(String, Double)]) = {
    val bfs = ss map { case (k, v) => BuiltFeature(k, v) }
    new FeatureReturn(bfs, false)
  }

  def fromValuedIntegers(ss: List[(Int, Double)]) = {
    val bfs = ss map { case (k, v) => BuiltFeature(k, v) }
    new FeatureReturn(bfs, false)
  }
}

class FeatureManager[Obs](
  val iString: String,
  val lex: Option[BloomLexicon],
  val wdProps: Option[WordProperties],
  val wdScores: Option[WordScores],
  val inducedFeatureMap: Option[InducedFeatureMap],
  val fnList: List[FeatureFn[Obs]]) extends Serializable

object FeatureFn {

  var fnId = 0

  def apply[Obs](f: (Int, SourceSequence[Obs], Int) => FeatureReturn) = {
    fnId += 1
    new FeatureFn[Obs]("fn_" + fnId) { def apply(s: Int, d: SourceSequence[Obs], p: Int) = f(s, d, p) }
  }

  def apply[Obs](s: String, f: FeatureFn[Obs], edgeP: Boolean, fcat: FeatureCat = StdFeature) = {
    new FeatureFn[Obs](s) {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = {
        val r = f(s, d, p)
        if (edgeP) r.setAsEdge()
        else {
          fcat match { case NNFeature => r.setAsNeural() case MultiFeature => r.setAsMulti() case _ => }
        }
        r
      }
    }
  }
}

/**
 * A feature function which subclasses <code>(Int,SourceSequence[Obs],Int) => FeatureReturn</code>.  Essentially,
 * this defines a function that takes three arguments, returns a <code>FeatureReturn</code> and
 * also has a string name.  Named feature functions are useful for tracing feature application
 * and provide a means to compare feature functions.
 * @param   top     <code>true</code> if this feature is to be registered in the list of
 *                  feature functions in the manager.
 * @param   n       A feature name as <code>String</code>
 */
abstract class FeatureFn[Obs](val n: String) extends Serializable {
  def this() = this("")

  override def toString = n

  def apply(s: Int, d: SourceSequence[Obs], p: Int): FeatureReturn

  val endCode: Long = IncrementalMurmurHash.hash("-END-", 0)
  val stCode: Long = IncrementalMurmurHash.hash("-START-", 0)
  val nullCode: Long = IncrementalMurmurHash.hash("-NULL-", 0)
  val winCode: Long = IncrementalMurmurHash.hash("WIN+", 0)
  val rngCode: Long = IncrementalMurmurHash.hash("Rng_", 0)

  def over(positions: Int*): FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>window<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = windowFn(rref, positions.toSeq)(s, d, p)
    }
  }

  def over(positions: Range): FeatureFn[Obs] = over(positions: _*)

  def within(positions: Int*): FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>windowWithin<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = windowAnyFn(rref, positions.toSeq)(s, d, p)
    }
  }
  def within(positions: Range): FeatureFn[Obs] = within(positions: _*)

  def ngram(suc: Option[String], positions: Int*): FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>ngram<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = ngramFn(rref, positions.toSeq, suc.isDefined)(s, d, p)
    }
  }
  def ngram(suc: Option[String], positions: Range): FeatureFn[Obs] = ngram(suc, positions: _*)
  def ngram(positions: Seq[Int]) = {
    val rref = this
    new FeatureFn[Obs](n + ">>ngram<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = ngramFn(rref, positions, false)(s, d, p)
    }
  }

  def displaced: FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>displaced<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = displacedFeatureFn(rref)(s, d, p)
    }
  }

  def cross(other: FeatureFn[Obs]): FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>cross<<" + other.n + " >><<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = crossProduct(List(rref), List(other))(s, d, p)
    }
  }

  def self(fname: String, positions: Int*): FeatureFn[Obs] = {
    val rref = this
    new FeatureFn[Obs](n + ">>self<<") {
      def apply(s: Int, d: SourceSequence[Obs], p: Int) = selfWinFn(rref, IncrementalMurmurHash.hash(fname, 0), positions.toSeq)(s, d, p)
    }
  }
  def self(fname: String): FeatureFn[Obs] = self(fname, 0)
  def self(fname: String, positions: Range): FeatureFn[Obs] = self(fname, positions: _*)

  /**
   * Computes a <code>FeatureReturn</code> with a single feature name/value pair
   * that is constructed by applying supplied feature function, <code>fn</code>,
   * to each position within the sequence at the specified sequence at relative offsets
   * and conjoining the feature name results.  So, for example,
   * <code>ngramFn((_wdFn _), Seq(-2,-1))(1,SourceSequence[Obs],5)</code> would generate a feature name
   * that consists of the observation at position 3 conjoined with the observation
   * at position 4 within the sequence <code>SourceSequence[Obs]</code>.
   * @param  fn         A supplied feature function
   * @param  positions  A sequence of <code>Int</code>s that specify relative positions to the current position
   * @param  s          Segment length
   * @param  sarr       SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos        Current position within the sequence
   * @return            Single conjunctive feature over relative positions.
   */
  def ngramFn(fn: FeatureFn[Obs], positions: Seq[Int], suc: Boolean)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val maxLen = sarr.length
    val fname = new BuiltFeature
    var failed = false
    positions foreach { p =>
      val rp = pos + p
      if ((rp >= 0) && (rp < maxLen)) {
        val localPairs = fn(s, sarr, rp).features
        if ((localPairs.size) < 1) if (!suc) failed = true else fname @@ nullCode
        localPairs foreach { s => fname @@@ s @@ p }
      } else if (rp == maxLen) fname @@ endCode
      else if (rp == -1) fname @@ stCode
    }
    if (failed) new FeatureReturn else new FeatureReturn(fname)
  }

  /**
   * Computes a <code>FeatureReturn</code> containing the set of feature name/value pairs
   * derived from the <i>cross product</i> of feature name/value pairs that result from
   * applying two sets of feature functions <code>fns1</code> and <code>fns2</code>. This provides
   * a way to specify conjunctive features over the features computed by other feature functions.
   * @param  fns1      A list of feature functions
   * @param  fns2      A second list of feature functions
   * @param  s         Segment length
   * @param  sarr      SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos       Current position within the sequence
   * @return           FeatureReturn with cross product of features over the two function lists.
   */
  def crossProduct(fns1: List[FeatureFn[Obs]], fns2: List[FeatureFn[Obs]])(s: Int, sarr: SourceSequence[Obs], pos: Int): FeatureReturn = {
    val npairs = new scala.collection.mutable.ListBuffer[BuiltFeature]
    for (fn1 <- fns1; fn2 <- fns2) {
      val fs1 = fn1(s, sarr, pos).features
      val fs2 = fn2(s, sarr, pos).features
      for (p1 <- fs1; p2 <- fs2)
        npairs += BuiltFeature(p1) @@@ p2
    }
    new FeatureReturn(npairs.toList, false)
  }

  /**
   * Computes a <code>FeatureReturn</code> with a set of feature value pairs
   * that are derived by applying supplied feature function, <code>fn</code>,
   * to each position within the sequence at the specified sequence at relative offsets.
   * @param  fn       A supplied feature function
   * @param  window   A sequence of <code>Int</code>s that specify relative positions to the current position
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return          Set of features from applying <code>fn> to <code>window</code>
   */
  def windowFn(fn: FeatureFn[Obs], window: Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val l1 = sarr.length
    window.foldLeft(new FeatureReturn) { (ac, rp) =>
      val cp = if (rp >= 0) pos + rp else pos - s + rp
      if ((cp >= 0) && (cp < l1)) {
        (fn(s, sarr, cp).update2(winCode, rp)) join ac
      } else ac
    }
  }

  def windowAnyFn(fn: FeatureFn[Obs], window: Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val l1 = sarr.length
    window.foldLeft(new FeatureReturn) { (ac, rp) =>
      val cp = if (rp >= 0) pos + rp else pos - s + rp
      if ((cp >= 0) && (cp < l1))
        (fn(s, sarr, cp) updateSI window update rngCode) join ac
      else ac
    }
  }

  def selfWinFn(fn: FeatureFn[Obs], name: Long, window: Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    throw new RuntimeException("Self window features now unsupported")
    /*
    inducedFeatureMap match { // if we have a map, now generate
      case Some(imap) =>
        val newSelfFs =
          if (inducingNewMap) { // might be inducing new self-features with an exsting inducedFeatureMap
            val fres = fn(s, sarr, pos).features
            new FeatureReturn(fres, false, true)
          } else new FeatureReturn
        val l1 = sarr.length
        window.foldLeft(newSelfFs) { (ac, rp) =>
          val cp = if (rp >= 0) pos + rp else pos - s + rp
          if ((cp >= 0) && (cp < l1)) {
            (genSelfFeatures(fn, name, imap, s, sarr, cp).update2(winCode, rp)) join ac
          } else ac
        }
      case None =>
        if (CrfInstance.training) new FeatureReturn
        else {
          // when decoding, if we have a selfFn, we should add the generated features to those that will be 'self-learned'
          val fres = fn(s, sarr, pos).features
          new FeatureReturn(fres, false, true)
        }
    }
    */
  }

  def displacedFeatureFn(fn: FeatureFn[Obs])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val fr = fn(s, sarr, pos)
    fr.displaced_=(true)
    fr
  }

}

/**
 * A <code>FeatureManager</code> defines includes a set of common feature function definitions.
 * It also holds a list of actual feature function objects that are applied to a sequence of
 * observations.  Sequence labeling applications will need to create a concrete subclass of
 * <code>FeatureManager</code> that specifies exactly which feature functions will be applied.
 * This class defines a simple DSL (Domain-Specific Language) that allows the set of feature
 * functions for a particular application to be clearly specified.
 * <p>
 * There are also higher-order
 * feature functions that take other feature functions as arguments to easily and compactly
 * specify more complicated feature extraction functionality.  The FeatureManager is type-parameterized
 * by <code>Obs</code> which represents the observation type and <code>Info</code> which
 * denotes the type of the auxilliary information (if any) associated with each observation.
 * <p>
 * An application-specific FeatureManager should subclass this class and specify, within the
 * body of the class definition a set of feature functions, where each function is described
 * as a single expression that returns an instance of <code>FeatureReturn</code>.  Below is an
 * example:
 * <p>
 * <pre>
 * object MyFeatureManager extends FeatureManager[String,Map[String,String] {
 *   "wdFn"      as wdFn
 *   "capRegFn"  as regexpFn("Capitalized", "[A-Z].*".r)
 *   "wdNgrm1"    as wdFn ngram (-2 to 0)
 *   "wdNgrm2"    as wdFn ngram (-1,0,1)
 *   "cross1"    as wdFn ngram (-1,0) cross (regexpFn("EndIn-ed",".*ed$".r) over (-2 to 2))
 * }
 * </pre>
 *
 * Each top-level function consists as a <code>String</code> followed by the keyword method
 * name "as" which is then followed by a feature function.  That feature function may be either 1) a
 * simple feature function such as <code>wdFn</code> or 2) a complex feature function created by
 * composing other feature functions.  For example, the feature function named "wdNgrm1" creates
 * an n-gram consisting of the concatenation of the features that result from applying the wdFn
 * feature function at the positions -2,-1 and 0 relative to the current position.  The "cross1"
 * feature function is a more complicated instance that takes the ngram computed from the words
 * at -1 and 0 and conjoins that feature with <i>all</i> the features that result from applying
 * the regular expression function that returns the feature name "EndIn-ed" (when its pattern is matched)
 * over the relative positions -2,-1,0,1,2.
 */
abstract class FeatureManagerBuilder[Obs](
  val lex: Option[BloomLexicon],
  val wdProps: Option[WordProperties],
  val wdScores: Option[WordScores],
  val inducedFeatureMap: Option[InducedFeatureMap],
  val iString: String,
  val inducingNewMap: Boolean) extends Serializable {

  type Fn = (Int, SourceSequence[Obs], Int) => FeatureReturn

  def getFeatureManager: FeatureManager[Obs] = {
    val fns = buildFeatureFns()
    new FeatureManager[Obs](iString, lex, wdProps, wdScores, inducedFeatureMap, fns)
  }

  def buildFeatureFns(s: String = "default"): List[FeatureFn[Obs]]

  /*
   * Especially dense features can cause numerical problems with long sequences.
   * A smaller weight for these features will help avoid overflow
   */
  val denseFeatureWt = 1.0

  def reset: Unit = {} // no-op by default, but can be used to for FeatureManager subclasses that hold global state per sequence

  implicit def toBuiltFeature(s: String) = BuiltFeature(s)

  type FnList = List[Fn] // may make this a set, so abstract type

  /**
   * Value for <code>_wdFn _</code>
   * @see #_wdFn(Int,SourceSequence[Obs],Int)
   */
  val wdFn = _wdFn _
  //val wdFn = new FeatureFn[Obs]("wdFn") { def apply(s: Int, sarr: SourceSequence[Obs], pos: Int) = new FeatureReturn(sarr(pos).code)}
  val nodeFn = _nodeFn _
  val edgeFn = _edgeFn _
  val regexpFn: (String, Regex) => Fn = { (fn, r) => _regexpFn(fn, r) _ }
  val lexFn: Fn = _lexiconFn(false) _
  val downLexFn: Fn = _lexiconFn(true) _
  val caselessWdFn: Fn = _caselessWdFn _

  val nodeFr = new FeatureReturn(":U:")
  val edgeFr = new FeatureReturn(":E:", true)
  val nodePrFr = new FeatureReturn(":U::")
  val edgePrFr = new FeatureReturn(":E::", true)

  // for efficiency, pre-hash these
  val numCode: Long = IncrementalMurmurHash.hash("-NUMBER-", 0)

  val selfWdCode: Long = IncrementalMurmurHash.hash("-selfWd-", 0)
  val wdScoreCode: Long = IncrementalMurmurHash.hash(":wdScore:", 0)

  /**
   * Computes a feature as the current observation
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return    A <code>FeatureReturn</code> with the observation feature as a hashcode
   */
  def _wdFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = new FeatureReturn(sarr(pos).code)

  /**
   * Computes a feature as the hashed conjunction of ALL labels produced from pre-models
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return    A <code>FeatureReturn</code> with the pre-model labels hash-conjoined
   */
  def _preLabFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val pc = sarr(pos).preLabelCode
    if (pc != 0L) new FeatureReturn(pc) else new FeatureReturn
  }

  val numRegex = """[0-9,.]*[0-9]+$""".r

  def wdFnNorm(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    numRegex.findFirstIn(sarr(pos).obs.toString) match {
      case Some(num) => new FeatureReturn(numCode)
      case None => new FeatureReturn(sarr(pos).code)
    }
  }

  /**
   * Computes a feature as the current observation ignoring case
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return    A <code>FeatureReturn</code> with the observation feature
   */
  def _caselessWdFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = new FeatureReturn(sarr(pos).obs.toString.toUpperCase)

  /**
   * Computes a feature with value ":U:"
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return     A <code>FeatureReturn</code> with the unknown word feature :U:
   */
  def _nodeFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = nodeFr

  /**
   * Computes a feature with value ":U::x" where x is the size of the current segment
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return     A <code>FeatureReturn</code> with the unknown word feature :U:
   */
  def _nodeFnSemi(s: Int, sarr: SourceSequence[Obs], pos: Int) = nodePrFr updateS s.toString

  /**
   * Computes a feature with value ":E:"
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return     A <code>FeatureReturn</code> with the unknown word feature :E: as an <i>edge</i> feature
   */
  def _edgeFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = edgeFr

  /**
   * Computes a feature with value ":E::x" where x is the size of the current segment
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return     A <code>FeatureReturn</code> with the unknown word feature :E: as an <i>edge</i> feature
   */
  def _edgeFnSemi(s: Int, sarr: SourceSequence[Obs], pos: Int) = edgePrFr updateS s.toString

  def _phraseAttributeFn(att: String)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    if ((pos - s) >= 0) {
      val sb = new StringBuilder
      var i = pos
      sb append "attrs="
      while (i >= (pos - s)) {
        sb append '_'
        sarr(i).info match { case Some(i) => i.foreach { case (k, v) => if (k.startsWith(att)) sb append v } case None => }
        i -= 1
      }
      new FeatureReturn(sb.toString)
    } else new FeatureReturn
  }

  def phraseWds(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    var fr = new FeatureReturn
    if ((pos - s) >= 0) {
      var i = pos - s
      while (i <= pos) {
        fr = fr.join(new FeatureReturn(sarr(i).obs.toString))
        i += 1
      }
      fr
    } else fr
  }

  def phraseFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    if ((pos - s) >= 0) {
      val sb = new StringBuilder
      var i = pos - s
      while (i <= pos) {
        if (i > (pos - s)) sb append ' '
        sb append sarr(i).obs.toString
        i += 1
      }
      new FeatureReturn(sb.toString)
    } else new FeatureReturn
  }

  def _phraseLexFn(aph: Boolean)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    if ((pos - s) >= 0) {
      val sb = new StringBuilder
      var i = pos - s
      while (i <= pos) {
        if (i > (pos - s)) sb append ' '
        sb append sarr(i).obs.toString
        i += 1
      }
      val phr = sb.toString
      lex match {
        case Some(l) =>
          val r: Option[List[Long]] = l.get(phr)
          r match { case Some(r) => new FeatureReturn(r map { v => BuiltFeature(v) }, false) case None => new FeatureReturn(phr) }
        case None => new FeatureReturn("ph=" + phr)
      }
    } else new FeatureReturn
  }

  def predicateFn(name: String, fns: List[FeatureFn[Obs]])(s: Int, sarr: SourceSequence[Obs], pos: Int) =
    if (fns exists { (fn: FeatureFn[Obs]) => fn(s, sarr, pos).features.size > 1 })
      new FeatureReturn(name)
    else new FeatureReturn

  def _lexiconFn(down: Boolean)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    new FeatureReturn(sarr(pos).lexCodes map { v => BuiltFeature(v, denseFeatureWt) }, false)
    /*
    case Some(l) =>
      val s = if (down) sarr(pos).obs.toString.toLowerCase else sarr(pos).obs.toString
      // lexicon features have a non-unit weight as they can be exceedingly "dense" - causing numerical issues
       case None => new FeatureReturn}
    case None => new FeatureReturn
    */
  }

  def wordPropertiesFn(down: Boolean)(s: Int, sarr: SourceSequence[Obs], pos: Int) = wdProps match {
    case Some(l) =>
      val s = sarr(pos).code
      l.get(s) match { case Some(r) => new FeatureReturn(r, denseFeatureWt) case None => new FeatureReturn }
    case None => new FeatureReturn
  }

  def wordPropertiesPrefixesFn(interval: Int, down: Boolean)(s: Int, sarr: SourceSequence[Obs], pos: Int) = wdProps match {
    case Some(l) =>
      val s = sarr(pos).code
      val fb = new collection.mutable.ListBuffer[String]
      l.get(s) match {
        case Some(r) =>
          r foreach { el =>
            val ln = el.length
            if (interval <= ln) fb += el.substring(0, interval)
          }
          new FeatureReturn(fb.toList) case None => new FeatureReturn
      }
    case None => new FeatureReturn
  }

  def wordScoresFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    wdScores match {
      case Some(l) =>
        l.get(sarr(pos).code) match {
          case Some(r) =>
            new FeatureReturn(wdScoreCode, r) case None => new FeatureReturn
        }
      case None => new FeatureReturn
    }
  }

  def allTagFn(s: Int, sarr: SourceSequence[Obs], pos: Int) = sarr(pos).infoCodes match {
    case Some(i) =>
      i.foldLeft(new FeatureReturn) { (ac, code) =>
        val nf = BuiltFeature(code)
        ac join nf
      }
    case None => new FeatureReturn
  }

  def attributeFn(att: String)(s: Int, sarr: SourceSequence[Obs], pos: Int) = sarr(pos).info match {
    case Some(i) =>
      new FeatureReturn((i.foldLeft(Nil: List[String]) { case (ac, kv) => (kv match { case (k, v) => if (k.startsWith(att)) (k + v) :: ac else ac }) }))
    case None => new FeatureReturn
  }

  def weightedAttributes(s: Int, sarr: SourceSequence[Obs], pos: Int) = sarr(pos).info match {
    case Some(i) =>
      FeatureReturn((i.foldLeft(Nil: List[(String, Double)]) {
        case (ac, vs) => vs match {
          case (k, v) => (k, (try { v.toDouble } catch { case _: Throwable => 1.0 })) :: ac
        }
      }))
    case None => new FeatureReturn
  }

  /**
   * Computes a <code>FeatureReturn</code> with a single feature value <code>fname</code>
   * if the observation at the current position matches the specified regular expression.
   * @param  fname    The name of the feature
   * @param  regexp   A regular expression applied to the observation
   * @param  s        Segment length
   * @param  sarr     SourceSequence of <code>ObsSource[Obs]</code> objects
   * @param  pos      Current position within the sequence
   * @return          Single feature if regexp matches current observation
   */
  def _regexpFn(fname: String, regexp: Regex)(s: Int, sarr: SourceSequence[Obs], pos: Int) =
    regexp.findFirstIn(sarr(pos).obs.toString) match {
      case Some(_) => new FeatureReturn(fname)
      case None => new FeatureReturn
    }

  private def substringsUpTo(init: Int, s: String, right: Boolean, pref: Boolean) = {
    val l = new scala.collection.mutable.ListBuffer[String]
    var i = init
    val l1 = s.length
    if ((l1 > 0) && ((init >= 0) || (init < l1))) {
      do {
        if (pref && i > 0 && i <= s.length) {
          val lb = new StringBuilder("PREF:")
          l += (lb append s.substring(0, i)).toString
        } else if (i >= 0 && i < s.length) {
          val lb = new StringBuilder("SUF:")
          l += (lb append s.substring(i, l1)).toString
        }
        if (right) i += 1 else i -= 1
      } while (i < l1 && i > 0)
    }
    l.toList
  }

  def prefixFn(size: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val prefixes = substringsUpTo(size, obsStr, false, true)
    new FeatureReturn(prefixes)
  }

  def suffixFn(size: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val suffixes = substringsUpTo(obsStr.length - size, obsStr, true, false)
    new FeatureReturn(suffixes)
  }

  def antiPrefixFn(size: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val aPrefixes = substringsUpTo(size, obsStr, false, false)
    new FeatureReturn(aPrefixes)
  }

  def antiSuffixFn(size: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val aSuffixes = substringsUpTo(obsStr.length - size, obsStr, true, true)
    new FeatureReturn(aSuffixes)
  }

  def prefNgrams(size: Int, range: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val eRng = range min obsStr.length
    val ngs = for (i <- 0 to (eRng - size)) yield obsStr.substring(i, i + size)
    new FeatureReturn(ngs.toList)
  }

  def sufNgrams(size: Int, range: Int)(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val obsStr = sarr(pos).obs.toString
    val obsLen = obsStr.length
    val st = (obsLen - range) max 0
    val ngs = for (i <- st until (obsLen - size)) yield obsStr.substring(i, i + size)
    new FeatureReturn(ngs.toList)
  }

  def sentPosition(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val pd = (pos.toDouble + 1.0)
    val fs = List(("sntRecip", (1.0 / pd)))
    FeatureReturn(if (pos == 0) ("-AtStart-", 1.0) :: fs else fs)
  }

  def wdLen(s: Int, sarr: SourceSequence[Obs], pos: Int) =
    new FeatureReturn("wdLen", (sarr(pos).obs.toString.length.toDouble))

  private def genSelfFeatures(fn: Fn, name: Long, imap: InducedFeatureMap, s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val fres = fn(s, sarr, pos).features
    if (fres.length == 1) {
      val weightedFs = imap.getWeightedFeatureVector(fres(0).get)
      weightedFs match {
        case Some(wfs) =>
          val bfs = Array.tabulate(wfs.length) { i => BuiltFeature(i, (wfs(i))) @@ name }
          new FeatureReturn(bfs.toList, false)
        case None => new FeatureReturn
      }
    } else new FeatureReturn
  }

  def nearestLeft(att: String, vl: String, cp: Int, sarr: SourceSequence[Obs]): Int = {
    var i = cp
    var done = false
    while (i > 0 && !done) {
      i -= 1
      sarr(i).info match {
        case Some(avTbl) => avTbl.get(att) match { case Some(v) => if (v == vl) done = true case None => }
        case None =>
      }
    }
    if (i < 0) cp else i
  }

  def nearestRight(att: String, vl: String, cp: Int, sarr: SourceSequence[Obs]): Int = {
    var i = cp
    var done = false
    val l1 = sarr.length - 1
    while (i < l1 && !done) {
      i += 1
      sarr(i).info match {
        case Some(avTbl) => avTbl.get(att) match { case Some(v) => if (v == vl) done = true case None => }
        case None =>
      }
    }
    if (i > l1) cp else i
  }

  def distanceToLeft(att: String, vl: String)(s: Int, sarr: SourceSequence[Obs], pos: Int): FeatureReturn = {
    val lpos = nearestLeft(att, vl, pos, sarr)
    val df = (pos - lpos)
    val pre = att + ":" + vl
    val fs =
      if (df <= 5) pre + "LeftWithin-" + df
      else if (df <= 7) pre + "LeftWithin-" + 7
      else if (df <= 9) pre + "LeftWithin-" + 9
      else if (df <= 12) pre + "LeftWithin-" + 12
      else pre + "LeftMoreThan-" + 12
    new FeatureReturn(fs)
  }

  def distanceToRight(att: String, vl: String)(s: Int, sarr: SourceSequence[Obs], pos: Int): FeatureReturn = {
    val rpos = nearestRight(att, vl, pos, sarr)
    val df = (rpos - pos)
    val pre = att + ":" + vl
    val fs =
      if (df <= 5) pre + "RightWithin-" + df
      else if (df <= 7) pre + "RightWithin-" + 7
      else if (df <= 9) pre + "RightWithin-" + 9
      else if (df <= 12) pre + "RightWithin-" + 12
      else pre + "RightMoreThan-" + 12
    new FeatureReturn(fs)
  }

}

object FeatureManagerBuilder {
  import scala.io.Source
  val utf8Codec = scala.io.Codec("utf-8")

  val defaultSpec = """wdFn as wdFn; lexFn as lexFn; unkFn as nodeFn; edgeFn as edgeFn;"""
    
  def isDynamic(s: String) = s.split(';').length > 1 

  /**
   * Build and set the lexicon if it has been provided and hasn't already been
   * created.
   */
  def getLexicon(opts: Options) = opts.lexDir map { d => new BloomLexicon(d) }

  def getWordProperties(opts: Options) = opts.wordPropFile map { f => WordProperties(f) }

  def getWordScores(opts: Options) = opts.wordScoreFile map { f => new WordScores(f) }

  def getInducedFeatureMap(opts: Options) = { opts.weightedFeatureMap map { InducedFeatureMap(_) } }

  def getFeatureSpecString(f: String) = {
    val sbuf = new StringBuffer
    Source.fromFile(new java.io.File(f)).getLines() foreach { sbuf.append(_) }
    sbuf.toString
  }
  
  def getMgr[Obs](fspec: String, l: Option[BloomLexicon], wdP: Option[WordProperties] = None, wdS: Option[WordScores] = None, iMap: Option[InducedFeatureMap] = None,
      inducing: Boolean = false) : FeatureManager[Obs] = {
    if (isDynamic(fspec)) {
      val dm = new DynamicFeatureManagerBuilder[Obs](l, wdP, wdS, iMap, fspec, inducing)
      dm.getFeatureManager  
    } else {
      val sm = new StaticFeatureManagerBuilder[Obs](fspec, l, wdP, wdS, iMap)
      sm.getFeatureManager
    }
  }

  def getMgrTrain[Obs](opts: Options): FeatureManager[Obs] = {
    val lexicon = getLexicon(opts)
    val wdProps = getWordProperties(opts)
    val wdScores = getWordScores(opts)
    val imap = getInducedFeatureMap(opts)
    val fspec = getFeatureSpecString(opts.featureSpec.get)
    getMgr(fspec, lexicon, wdProps, wdScores, imap)
  }

  def getMgrDecode[Obs](opts: Options, model: StdModel, pre: Boolean): FeatureManager[Obs] = {
    val lexicon = if (model.lex.isDefined) model.lex else if (!pre) getLexicon(opts) else None
    val wdProps = if (model.wdProps.isDefined) model.wdProps else if (!pre) getWordProperties(opts) else None
    val wdScores = if (model.wdScores.isDefined) model.wdScores else if (!pre) getWordScores(opts) else None
    val imap = if (model.inducedFs.isDefined) model.inducedFs else if (!pre) getInducedFeatureMap(opts) else None
    getMgr(model.fspec, lexicon, wdProps, wdScores, imap)    
  }

  def createForSelfTraining[Obs](opts: Options, model: Model): FeatureManager[Obs] = {
    // use provided feature spec, if it's given
    // this is done so that additional self-features can be added to the feature spec without having to re-train underlying model
    val lexicon = getLexicon(opts)
    val wdProps = getWordProperties(opts)
    val wdScores = getWordScores(opts)
    val imap = getInducedFeatureMap(opts)
    val fspec = opts.featureSpec match { case Some(fs) => getFeatureSpecString(fs) case None => model.fspec }
    getMgr(fspec, lexicon, wdProps, wdScores, imap, true)
  }

  def apply[Obs](opts: Options): FeatureManager[Obs] = {
    opts.featureSpec match {
      case Some(m) => getMgrTrain[Obs](opts)
      case None =>
        val lexicon = getLexicon(opts)
        val m = new DynamicFeatureManagerBuilder[Obs](lexicon, None, None, None, defaultSpec)
        m.getFeatureManager
    }
  }

  def apply[Obs](opts: Options, m: StdModel, pre: Boolean = false): FeatureManager[Obs] = getMgrDecode[Obs](opts, m, pre)

}

class StaticFeatureManagerBuilder[Obs](
  val staticName: String = "default",
  l: Option[BloomLexicon] = None,
  wdP: Option[WordProperties] = None,
  wdS: Option[WordScores] = None,
  iMap: Option[InducedFeatureMap] = None,
  induceMap: Boolean = false) extends FeatureManagerBuilder[Obs](l, wdP, wdS, iMap, staticName, induceMap) {
  
  override def getFeatureManager = {
    val fns = buildFeatureFns(staticName)
    new FeatureManager[Obs](iString, lex, wdProps, wdScores, inducedFeatureMap, fns)
  }

  def buildFeatureFns(knownFset: String = "default") = {
    knownFset match {
      case "default" =>
        List(
          FeatureFn(nodeFn),
          FeatureFn(edgeFn),
          FeatureFn(wdFn),
          FeatureFn(lexFn),
          FeatureFn(wdFn).ngram(None, (-1 to 0)),
          FeatureFn(lexFn).over((-1 to 1)),
          FeatureFn(wdFn).over((-2 to 2)),
          FeatureFn(wordPropertiesFn(false)) over (-2 to 2))
      case "en-pos" =>        
        List(
          FeatureFn(nodeFn),
          FeatureFn(edgeFn),
          FeatureFn(downLexFn).over((-2 to 2)),
          FeatureFn(wdFnNorm).over((-2 to 2)),
          FeatureFn("trans1",FeatureFn(wdFnNorm).over((-1 to 1)),true),
          FeatureFn(wdFnNorm).ngram(None,(-1 to 0)),
          FeatureFn(wdFnNorm).ngram(None,(0 to 1)),
          FeatureFn("clusterTr1",FeatureFn(wordPropertiesPrefixesFn(4,false)),true),
          FeatureFn("clusterTr2",FeatureFn(wordPropertiesPrefixesFn(6,false)),true),
          FeatureFn("clusterTr3",FeatureFn(wordPropertiesPrefixesFn(8,false)),true),
          FeatureFn(wordPropertiesPrefixesFn(4,false)).over((-2 to 2)),
          FeatureFn(wordPropertiesPrefixesFn(6,false)).over((-2 to 2)),
          FeatureFn(wordPropertiesPrefixesFn(8,false)).over((-2 to 2)),
          FeatureFn(wordPropertiesPrefixesFn(4,false)).ngram(None,(-1 to 0)),
          FeatureFn(wordPropertiesPrefixesFn(6,false)).ngram(None,(-1 to 0)),
          FeatureFn(wordPropertiesPrefixesFn(8,false)).ngram(None,(-1 to 0)),          
          FeatureFn(wordPropertiesFn(false)).over((-2 to 2)),
          FeatureFn(wordPropertiesFn(false)).ngram(None,(-1 to 0)),
          FeatureFn(prefixFn(8)),
          FeatureFn(suffixFn(8)),
          FeatureFn(regexpFn("INITCAP","^[A-Z].*".r)).over((-3 to 4)),
          FeatureFn(regexpFn("INITCAP","^[A-Z].*".r)).ngram(None,(1 to 2)),
          FeatureFn(regexpFn("INITCAP","^[A-Z].*".r)).ngram(None,(-2 to -1)),
          FeatureFn(regexpFn("INITCAP_ALPHA","^[A-Z][a-z]*$".r)).over((-2 to 2)),
          FeatureFn(regexpFn("ALLCAPS","^[A-Z]+$".r)).over((-2 to 2)),
          FeatureFn(regexpFn("CAPSMIX","^[A-z]+$".r)),
          FeatureFn(regexpFn("HASDIGIT",".*[0-9].*".r)),
          FeatureFn(regexpFn("HASDASH",".*-.*".r)).over(-1 to 1),
          FeatureFn(regexpFn("INITDASH","^-.*".r)).over(-1 to 1),
          FeatureFn(regexpFn("PUNCT","^[^A-z0-9]+$".r))
            )
      case "en-pos-2" =>        
        List(
          FeatureFn(nodeFn),
          FeatureFn(edgeFn),
          FeatureFn(wdFnNorm).over((-2 to 2)),
          FeatureFn(downLexFn),          
          FeatureFn(lexFn).over((-1 to 1)),
          FeatureFn(wdFn).over((-3 to 3)),
          FeatureFn(wordPropertiesFn(false)) over (-2 to 2),
          FeatureFn(wdFn).ngram(Seq(-1,1)),
          FeatureFn(wdFn).ngram(Seq(-1,0)),
          FeatureFn(wdFn).ngram(Seq(-2,-1)),
          FeatureFn(wdFn).ngram(Seq(1,2)),
          FeatureFn(wdFn).ngram(Seq(-2,-1,0)),
          FeatureFn(caselessWdFn).over(-2 to 2),
          FeatureFn(prefixFn(5)).over(-2 to 2),
          FeatureFn(suffixFn(5)).over(-2 to 2),
          FeatureFn(regexpFn("INITCAP","^[A-Z].*".r)),
          FeatureFn(regexpFn("INITCAP_ALPHA","^[A-Z][a-z]*$".r)),
          FeatureFn(regexpFn("ALLCAPS","^[A-Z]+$".r)),
          FeatureFn(regexpFn("CAPSMIX","^[A-z]+$".r)),
          FeatureFn(regexpFn("HASDIGIT",".*[0-9].*".r)),
          FeatureFn(regexpFn("HASDASH",".*-.*".r)),
          FeatureFn(regexpFn("INITDASH","^-.*".r)),
          FeatureFn(regexpFn("PUNCT","^[^A-z0-9]+$".r))
            )
      case "wd-only" =>
        List(
            FeatureFn(nodeFn),
            FeatureFn(edgeFn),
            FeatureFn(wdFn)
            )
      case "wd-and-context" =>
        List(
            FeatureFn(nodeFn),
            FeatureFn(edgeFn),
            FeatureFn(wdFn),
            FeatureFn(wdFn) over (-2 to 2)
            )
      case a =>
        throw new RuntimeException("Unknown static (pre-defined) feature set: " + a)
    }

  }
}

object IncrementalMurmurHash extends Serializable {
  val m = 0xc6a4a7935bd1e995L
  val r = 47

  private val numRx = """[0-9]+$|[0-9]+\.[0-9]+$|[0-9]+[0-9,]+$""".r

  def isNumberString(s: String) = numRx.findPrefixOf(s).isDefined
  //def isNumberString(s: String) = false

  val numSpecialHash = hash("::NUMBER::")

  def get(l: Long): Long = mix(0L, l)

  // single step mix
  def mix(h_i: Long, k_i: Long): Long = {
    var h = h_i
    var k = k_i
    k *= m
    k ^= k >>> r
    k *= m
    h *= m
    h ^= k
    h
  }

  def mix(s: String, k: Long): Long = mix(hash(s), k)

  def hash(data: String): Long = hash(data, 0)

  def hash(data: String, seed: Long): Long = {
    val bytes = data.getBytes("UTF-16")
    hash(bytes, bytes.length, seed)
  }

  def hash(data: Array[Byte]): Long = {
    hash(data, data.length, 0)
  }

  def hash(data: Array[Byte], l: Int, seed: Long): Long = {
    var len = l
    val len_8 = len / 8
    var h: Long = (seed & 0xffffffffl) ^ (l * m)
    var i = 0
    while (i < len_8) {
      val i8 = i * 8
      var k: Long =
        ((data(i8 + 0) & 0xff).asInstanceOf[Long] +
          ((data(i8 + 1) & 0xff).asInstanceOf[Long] << 8) +
          ((data(i8 + 2) & 0xff).asInstanceOf[Long] << 16) +
          ((data(i8 + 3) & 0xff).asInstanceOf[Long] << 24) +
          ((data(i8 + 4) & 0xff).asInstanceOf[Long] << 32) +
          ((data(i8 + 5) & 0xff).asInstanceOf[Long] << 40) +
          ((data(i8 + 6) & 0xff).asInstanceOf[Long] << 48) +
          ((data(i8 + 7) & 0xff).asInstanceOf[Long] << 56))
      k *= m
      k ^= k >>> r
      k *= m
      h *= m
      h ^= k
      i += 1
    }
    val rm = l % 8

    if (rm >= 7) h ^= (data((l & ~7) + 6).asInstanceOf[Long] & 0xff) << 48
    if (rm >= 6) h ^= (data((l & ~7) + 5).asInstanceOf[Long] & 0xff) << 40
    if (rm >= 5) h ^= (data((l & ~7) + 4).asInstanceOf[Long] & 0xff) << 32
    if (rm >= 4) h ^= (data((l & ~7) + 3).asInstanceOf[Long] & 0xff) << 24
    if (rm >= 3) h ^= (data((l & ~7) + 2).asInstanceOf[Long] & 0xff) << 16
    if (rm >= 2) h ^= (data((l & ~7) + 1).asInstanceOf[Long] & 0xff) << 8
    if (rm >= 1) h ^= (data(l & ~7).asInstanceOf[Long] & 0xff)
    h *= m;
    // mix 1
    h ^= h >>> r
    h *= m
    h ^= h >>> r
    h
  }

}
