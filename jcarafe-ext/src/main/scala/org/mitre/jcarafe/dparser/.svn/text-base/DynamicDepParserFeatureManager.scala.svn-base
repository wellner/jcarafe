/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.dparser
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.syntax._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import scala.io.Source

class DynamicDepParserFeatureManager(iString: String, mxStates:Int) extends DepParserFeatureManager(iString, mxStates) with RegexParsers {
  val codec = scala.io.Codec("utf-8")

  def getFeatureSpecString(f:String) = {
    val sbuf = new StringBuffer
    Source.fromFile(new java.io.File(f)).getLines() foreach {sbuf.append(_)} 
    sbuf.toString
  }

  def topExprs : Parser[List[FeatureFn]] = repsep(topExpr,"""[;\r\n]+""".r) ~ opt(";") ^^ {case (e~_) => e}
  def topExpr : Parser[FeatureFn] = fullExpr ^^ {e => e}

  def fullExpr: Parser[FeatureFn] = simpleFnExpr ^^ {e => e}

  def simpleFnExpr: Parser[FeatureFn] =
    _basicFns2 | _basicFns | _prefixFns | _inBetweenFns | _surroundingWordPosFns | _unigramFns | _bigramFns | _surroundingWordCoarsePosFns

  def _basicFns : Parser[FeatureFn] = "basicFns" ^^ {_ => basicFns _}

  def _basicFns2 : Parser[FeatureFn] = "basicFns2" ^^ {_ => basicFns2 _}
  def _prefixFns : Parser[FeatureFn] = "prefixFns" ^^ { _ => unigramPrefixFns _}
  def _inBetweenFns : Parser[FeatureFn] = "inBetweenFns" ^^ { _ => inBetweenFns _}
  def _surroundingWordPosFns : Parser[FeatureFn] ="surroundingWordPosFns" ^^ { _ => surroundingWordPosFns _}
  def _surroundingWordCoarsePosFns : Parser[FeatureFn] ="surroundingWordCoarsePosFns" ^^ { _ => surroundingWordCoarsePosFns _}
  def _unigramFns : Parser[FeatureFn] = "unigramFns" ^^ { _ => unigramFns _}
  def _bigramFns : Parser[FeatureFn] = "bigramFns" ^^ { _ => bigramFns _}
  
  

  def parseIt(r: java.io.Reader) : List[FeatureFn] = { 
    val res = parseAll(topExprs,new PagedSeqReader(PagedSeq.fromReader(r)))
    if (res.successful) res.get else throw new RuntimeException("Feature Specification File failed to Parse") }
  
  val fineFnList = parseIt(new java.io.StringReader(getFeatureSpecString(iString)))

}
