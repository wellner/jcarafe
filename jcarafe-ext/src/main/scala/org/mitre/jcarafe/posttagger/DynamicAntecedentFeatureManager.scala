/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.syntax._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import scala.io.Source
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._

class DynamicAntecedentFeatureManager(mxStates:Int, iString: String) extends AntecedentFeatureManager(iString,mxStates) with RegexParsers {

  import DynamicAntecedentFeatureManager.getSpecString

    
  def topExprs : Parser[List[FeatureFn]] = repsep(topExpr,"""[;\r\n]+""".r) ~ opt(";") ^^ {case (e~_) => e}
  def topExpr : Parser[FeatureFn] = fname ~ ("as" | ":") ~ fullExpr ^^ {case (l~_~f) => f}  
  def fname : Parser[String] = """[A-z0-9_\.:-]+""".r
  
  def fullExpr : Parser[FeatureFn] = (simpleFnExpr ~ ("cross"|"X") ~ fullExpr) ^^ {case (l~_~r) => l.cross(r)} | simpleFnExpr
  
  def simpleFnExpr : Parser[FeatureFn] = 
    overlap2Fn | prevIsAnnounceFn | DAPairFn | binnedHypLengthFn | binnedLengthFn | lexOverlapFn | distFn | refLexFn | hypLexFn | userFn | announceFn | mentionFn | lenFn | previousLengthFn | lengthRatioFn | endingCharsFn | overlapFn
  
  def distFn : Parser[FeatureFn] = "distFn" ^^ {_ => _distFn _}
  def userFn : Parser[FeatureFn] = "userFn" ^^ {_ => _sameUserFn _}
  def overlapFn : Parser[FeatureFn] = "overlapFn" ^^ {_ => _overlapFn _}
  def previousLengthFn : Parser[FeatureFn] = "previousLengthFn" ^^ {_ => _previousLengthFn _}
  def lengthRatioFn : Parser[FeatureFn] = "lengthRatioFn" ^^ {_ => _lengthRatioFn _}
  def endingCharsFn : Parser[FeatureFn] = "endingCharsFn" ^^ {_ => _endingCharsFn _}
  def announceFn : Parser[FeatureFn] = "announceFn" ^^ {_ => _isAnnounce _}
  def mentionFn : Parser[FeatureFn] = "mentionFn" ^^ { _ => _isAddressed _ }
  def binnedHypLengthFn : Parser[FeatureFn] = "binnedHypLengthFn" ^^ { _ => _binnedHypLength _}
  def binnedLengthFn : Parser[FeatureFn] = "binnedLengthFn" ^^ { _ => _binnedLength _}
  def lexOverlapFn : Parser[FeatureFn] = "lexOverlapFn" ^^ { _ => _lexOverlapFn _}
  def refLexFn : Parser[FeatureFn] = "refLexFn" ^^ { _ => _refLexFn _}
  def hypLexFn : Parser[FeatureFn] = "hypLexFnFn" ^^ { _ => _hypLexFn _}
  def overlap2Fn : Parser[FeatureFn] = "overlap2Fn" ^^ { _ => _overlap2Fn _}
  def prevIsAnnounceFn : Parser[FeatureFn] = "prevIsAnnounceFn" ^^ { _ => _prevIsAnnounce _}
  def DAPairFn : Parser[FeatureFn] = "DAPairFn" ^^ { _ => _DAPair _}
  def lenFn : Parser[FeatureFn] = "lenFn" ^^ { _ => _length _}

  def parseIt(r: java.io.Reader) : List[FeatureFn] = { 
    val res = parseAll(topExprs,new PagedSeqReader(PagedSeq.fromReader(r)))
    if (res.successful) res.get else throw new RuntimeException("Feature Specification File failed to Parse") }
  
  val fineFnList = parseIt(new java.io.StringReader(iString))
  
}


object DynamicAntecedentFeatureManager {
  def getSpecString(f:String) = {
    val sbuf = new StringBuffer
    val codec = scala.io.Codec("utf-8")
    Source.fromFile(new java.io.File(f)).getLines() foreach {sbuf.append(_)} 
    sbuf.toString
  }

  def apply(mx: Int, s: String) = new DynamicAntecedentFeatureManager(mx, getSpecString(s))
}
