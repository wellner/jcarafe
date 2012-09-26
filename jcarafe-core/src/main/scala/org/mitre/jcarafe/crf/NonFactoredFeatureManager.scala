/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

abstract class NonFactoredFeatureManager[Obs](val iString: String) { 

  type FineFn = (Int, SourceSequence[Obs], Int, Int) => FeatureReturn

  type Fn = (Int, SourceSequence[Obs], Int) => FeatureReturn
  
  var lex: Option[BloomLexicon] = None
  
    /**
   * Optional listing of properties for each word
   */
  var wdProps: Option[WordProperties] = None

  
  abstract class FeatureFn(val name: String) extends FineFn {
    def this() = this("")
 
    def cross(other: FeatureFn) : FeatureFn = {
      val rref = this
      new FeatureFn(name+">>cross<<" + other.name + " >><<") {
    	def apply(s:Int,d:SourceSequence[Obs],p:Int,state:Int) = crossProduct(List(rref),List(other)) (s,d,p,state) }}
  }
  
  // this conveniently maps occurrences of FineFn to FeatureFn instances
  implicit def upgrad(f: FineFn) : FeatureFn = new FeatureFn {def apply(s:Int,d:SourceSequence[Obs],p:Int,state:Int) = f(s,d,p,state)}
  
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
  def crossProduct(fns1:Seq[FineFn], fns2: Seq[FineFn])(s: Int, sarr: SourceSequence[Obs], pos: Int, state: Int) : FeatureReturn = {
    val npairs = new scala.collection.mutable.ListBuffer[BuiltFeature]
    for (fn1 <- fns1; fn2 <- fns2 ) 
      for (p1 <- fn1(s,sarr,pos,state).features; p2 <- fn2(s,sarr,pos,state).features) 
        npairs += ((p1 @@@ p2))
    new FeatureReturn(npairs.toList,false)
  }
  
  val fineFnList : Seq[FineFn]  
  val fnList : Seq[Fn]
}

