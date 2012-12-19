/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.syntax._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.mitre.jcarafe.util._

/*
 * A DynamicFeatureManager is able to load feature extraction functions
 * at <i>runtime</i> by reading from a feature extraction specification file.
 * This allows the same piece of software to accomodate very different tagging
 * and sequence labeling tasks.  Crucially, it allows users to customize
 * feature extraction functionality without needing to recompile.
 * 
 * @author Ben Wellner
*/
class DynamicFeatureManager[Obs](iString: String) extends FeatureManager[Obs](iString) with RegexParsers {

  var ii = 0
  def genSym = {
    val r = "f_"+ii
    ii += 1
    r
  }
  parseString(iString)
  
  def topExprs : Parser[List[FeatureFn]] = repsep(topExpr,"""[;\r\n]+""".r) ~ ";" ^^ {case (e~_) => e filter {_.isDefined} map {_.get}}
  
  def topExpr : Parser[Option[FeatureFn]] = topExpr0 | topExpr1 | topExpr2
  
  def topExpr0 : Parser[Option[FeatureFn]] = comment ^^ {_ => None}

  def topExpr1 : Parser[Option[FeatureFn]] = 
    fname ~ ("as" | ":") ~ fullExpr ~ opt("TRANSITION"|"NEURAL"|"MULTI") ~ opt(comment) ^^ 
    {case (l~_~f~t~_) => 
      val fFun = t match {case Some("TRANSITION") => FeatureFn(l,f,true) 
				case Some("NEURAL") => FeatureFn(l,f,false,NNFeature) 
				case Some("MULTI") => FeatureFn(l,f,false,MultiFeature)
				case _ => FeatureFn(l,f,false)}
      Some(fFun)
      }
  
  def topExpr2 : Parser[Option[FeatureFn]] = 
    fullExpr ~ opt("TRANSITION"|"NEURAL"|"MULTI") ~ opt(comment) ^^ 
    {case (f~t~_) =>
      val l = genSym
      val fFun = t match {
      	case Some("TRANSITION") => FeatureFn(l,f,true) 
      	case Some("NEURAL") => FeatureFn(l,f,false,NNFeature) 
      	case Some("MULTI") => FeatureFn(l,f,false,MultiFeature)
      	case _ => FeatureFn(l,f,false)}
      Some(fFun)
      }  
  
  def comment : Parser[String] = """#.*""".r

  def fname : Parser[String] = """[A-z0-9_\.:-]+""".r
  
  def fullExpr : Parser[FeatureFn] = 
    (cfnExpr ~ ("cross"|"X") ~ fullExpr) ^^ {case (l~_~r) => l.cross(r)} |
    (simpleFnExpr ~ ("cross"|"X") ~ fullExpr) ^^ {case (l~_~r) => l.cross(r)} | 
    ("(" ~ cfnExpr ~ ")" ~ ("cross"|"X") ~ "(" ~ fullExpr ~ ")") ^^ {case (_~l~_~_~_~r~_) => l.cross(r)} |
    ("(" ~ simpleFnExpr ~ ")" ~ ("cross"|"X") ~ "(" ~ fullExpr ~ ")") ^^ {case (_~l~_~_~_~r~_) => l.cross(r)} | 
    cfnExpr
  
  def cfnExpr : Parser[FeatureFn] =  
    (simpleFnExpr ~ ("ngram"|"N-") ~ rangeExpr ~ opt("SUCCEED")) ^^ {case (l~_~r~o) => l.ngram(o,r:_*)} |
    (simpleFnExpr ~ ("within"|"W") ~ rangeExpr) ^^ {case (l~_~r) => l.within(r:_*)} |
    (simpleFnExpr ~ ("over"|"@") ~ rangeExpr)   ^^ {case (l~_~r) => l.over(r:_*)} |
    (simpleFnExpr ~ ("self"|"<>") ~ fname ~ opt(rangeExpr)) ^^ {case (l~_~r~s) => s match {case None => l.self(r) 
											   case Some(s) => l.self(r,s:_*)}} |
    ("(" ~ fullExpr ~ ")" ~ ("ngram"|"N-") ~ rangeExpr ~ opt("SUCCEED")) ^^ {case (_~l~_~_~r~o) => l.ngram(o,r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("within"|"W-") ~ rangeExpr) ^^ {case (_~l~_~_~r) => l.within(r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("over"|"@-") ~ rangeExpr) ^^ {case (_~l~_~_~r) => l.over(r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("displaced")) ^^ { case (_~l~_~_) => l.displaced } |
    ("(" ~ fullExpr ~ ")" ~ ("self"|"<>") ~ fname ~ opt(rangeExpr)) ^^ { case (_~l~_~_~r~s) => s match {case None => l.self(r) 
												      case Some(s) => l.self(r,s:_*) }} |
    simpleFnExpr
  
  def simpleFnExpr : Parser[FeatureFn] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | caseLessFnExpr | lexFnExpr | wdPropFnExpr | downWdPropFnExpr | wdScoreFnExpr |
    downLexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | attributeFnExpr |
    weightedAttrExpr | distToLeftExpr | distToRightExpr | nodeFnSemiExpr | edgeFnSemiExpr | phraseFnExpr | semiAttributeFnExpr | phraseWdsExpr |
    prefNGramExpr | sufNGramExpr | sentPosExpr | wdLenExpr | wdFnNormExpr | wdPropPrefixFnExpr | posFnExpr 
  
  def wdFnExpr : Parser[FeatureFn]           = "wdFn" ^^ {_ => wdFn}
  def wdFnNormExpr : Parser[FeatureFn]       = "wdNormFn" ^^ {_ => wdFnNorm _}
  def caseLessFnExpr : Parser[FeatureFn]     = "caselessWdFn" ^^ {_ => caselessWdFn}
  def posFnExpr: Parser[FeatureFn]           = "preLabFn" ^^ {_ => _preLabFn _}
  def lexFnExpr : Parser[FeatureFn]          = "lexFn" ^^ {_ => lexFn}
  def downLexFnExpr : Parser[FeatureFn]      = "downLexFn" ^^ {_ => downLexFn}
  def wdPropFnExpr : Parser[FeatureFn]       = "wdPropFn" ^^ {_ => wordPropertiesFn(false) _ }
  def wdPropPrefixFnExpr : Parser[FeatureFn] = "wdPropPrefixFn(" ~> intVal <~ ")" ^^ {v => wordPropertiesPrefixesFn(v,false) _ }
  def downWdPropFnExpr : Parser[FeatureFn]   = "downWdPropFn" ^^ {_ => wordPropertiesFn(true) _ }
  def wdScoreFnExpr : Parser[FeatureFn]      = "wdScoreFn" ^^ {_ => wordScoresFn _ }
  def nodeFnExpr : Parser[FeatureFn]         = "nodeFn" ^^ {_ => nodeFn}
  def nodeFnSemiExpr : Parser[FeatureFn]     = "semiNodeFn" ^^ {_ => _nodeFnSemi _}
  def edgeFnSemiExpr : Parser[FeatureFn]     = "semiEdgeFn" ^^ {_ => _edgeFnSemi _}
  def semiAttributeFnExpr: Parser[FeatureFn] = "semiAttrFn(" ~> fname <~ ")" ^^ {v => _phraseAttributeFn(v) _}
  def phraseFnExpr : Parser[FeatureFn]       = "phraseFn" ^^ {_ => phraseFn _}
  def phraseWdsExpr : Parser[FeatureFn]      = "phraseWds" ^^ {_ => phraseWds _}
  def edgeFnExpr : Parser[FeatureFn]         = "edgeFn" ^^ {_ => edgeFn}
  def allTagFnExpr : Parser[FeatureFn]       = "allTagFn" ^^ {_ => allTagFn _}
  def weightedAttrExpr : Parser[FeatureFn]   = "weightedAttrs" ^^ {_ => weightedAttributes _}
  def distToLeftExpr : Parser[FeatureFn]     = "distToLeft(" ~> fname ~ "," ~ fname <~ ")" ^^ {case (a~_~v) => distanceToLeft(a,v) _ }
  def distToRightExpr : Parser[FeatureFn]    = "distToRight(" ~> fname ~ "," ~ fname <~ ")" ^^ {case (a~_~v) => distanceToRight(a,v) _ }
  def sentPosExpr : Parser[FeatureFn]        = "sentPos" ^^ {_ => sentPosition _}
  
  def prefFnExpr : Parser[FeatureFn]         = "prefixFn(" ~> intVal <~ ")" ^^ { v => prefixFn(v) _}

  def attributeFnExpr : Parser[FeatureFn]    = "attributeFn(" ~> fname <~ ")" ^^ {v => attributeFn(v) _}
  def sufFnExpr : Parser[FeatureFn]          = "suffixFn(" ~> intVal <~ ")" ^^ { v => suffixFn(v) _}
  def antiPrefFnExpr : Parser[FeatureFn]     = "antiPrefixFn(" ~> intVal <~ ")" ^^ { v => antiPrefixFn(v) _}
  def antiSufFnExpr : Parser[FeatureFn]      = "antiSuffixFn(" ~> intVal <~ ")" ^^ { v => antiSuffixFn(v) _}
  def prefNGramExpr : Parser[FeatureFn]      = "prefNGrams(" ~> intVal ~ "," ~ intVal <~ ")" ^^ {case (s~_~r) => prefNgrams(s,r) _ }
  def sufNGramExpr : Parser[FeatureFn]       = "sufNGrams(" ~> intVal ~ "," ~ intVal <~ ")" ^^ {case (s~_~r) => sufNgrams(s,r) _ }
  def wdLenExpr : Parser[FeatureFn]          = "wdLen" ^^ {_ => wdLen _}

  def regexpFnExpr : Parser[FeatureFn]       = "regexpFn(" ~> fname ~ "," ~ """(\\\)|[^\)])+""".r <~ ")" ^^ 
                                            {case (fn~_~e) => 
					      val parenMap = """\\\)""".r // use to map escaped parens to actual parens
					      val ne = parenMap.replaceAllIn(e,{_ => ")"})
					      _regexpFn(fn,ne.r) _}
  def predicateExpr : Parser[FeatureFn]      = "predicateFn(" ~ fname ~ "," ~ repsep(simpleFnExpr,",") ~ ")" ^^ {case (_~fn~_~fs~_) => predicateFn(fn,fs) _}
  
  def rangeExpr : Parser[Seq[Int]] = basRangeExpr | toRangeExpr
  def basRangeExpr : Parser[Seq[Int]] = "(" ~> repsep(intVal,",") <~ ")" ^^ {_.toSeq}
  def toRangeExpr : Parser[Seq[Int]] = "(" ~> intVal ~ "to" ~ intVal <~ ")" ^^ {case(f~_~t) => (f to t).toSeq} 
  def intVal : Parser[Int] = """-?[0-9]+""".r ^^ {_.toInt}
  def trueVal : Parser[Boolean] = "true" ^^ {_ => true}
  def falseVal : Parser[Boolean] = "false" ^^ {_ => false}
  def boolVal : Parser[Boolean] = trueVal | falseVal

  def parseIt(r: java.io.Reader) : List[FeatureFn] = { 
    val res = parseAll(topExprs,new PagedSeqReader(PagedSeq.fromReader(r)))
    if (res.successful) res.get else throw new RuntimeException("Feature Specification File failed to Parse") }
  def parseString(s: String) : Unit = parseIt(new java.io.StringReader(s)) foreach {f => fnBuf += f}
  
}

/** 
 * This dynamic feature manager redefines window and ngram functions to 
 * operate over the original sequence of elements in the context where 
 * we've recoded the original sequence to a selected sub-sequence with
 * re-mapped labels.
 * <i>Design note: This could be a trait also</i>
 */
class DynamicRecodeFeatureManager[Obs](iString: String) extends DynamicFeatureManager[Obs](iString) {
  
  override def simpleFnExpr : Parser[FeatureFn] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | lexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | recodeFnExpr
  
  def recodeFnExpr : Parser[FeatureFn] = betweenPrevExpr | betweenSubExpr | distancePrevious | distanceSubsequent 
  
  def betweenPrevExpr : Parser[FeatureFn] = "betweenPrevious(" ~> cfnExpr <~ ")" ^^ {fn => betweenPrevFn(fn) _}
  def betweenSubExpr : Parser[FeatureFn] = "betweenSubsequent(" ~> cfnExpr <~ ")" ^^ {fn => betweenSubsequentFn(fn) _}
  def distancePrevious : Parser[FeatureFn] = "distancePrevious" ^^ { _ => distancePrev _}
  def distanceSubsequent : Parser[FeatureFn] = "distanceSubsequent" ^^ { _ => distancePrev _}
    
  override def windowFn(fn:Fn, window:Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = 
    sarr.parentSeq match {
      case Some(pSeq) =>
        val st = sarr(pos).start
        val en = sarr(pos).end
        window.foldLeft(new FeatureReturn){(ac,rp) =>
        val cp = if (rp >= 0) en + rp else st + rp 
        val up = "DWIN@" + rp.toString
        if ((cp >= 0) && (cp < pSeq.length)) 
        	(fn(s,pSeq,cp) updateS up) join ac
        else ac}
      case None => new FeatureReturn
    }	
  override def windowAnyFn(fn:Fn, window:Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val pref = "DWIN@"+window.toString
    sarr.parentSeq match {
      case Some(pSeq) =>
        val st = sarr(pos).start
        val en = sarr(pos).end
        window.foldLeft(new FeatureReturn){(ac,rp) =>
        val cp = if (rp >= 0) en + rp else st + rp 
        if ((cp >= 0) && (cp < pSeq.length)) {
        	(fn(s,pSeq,cp) updateS pref) join ac
         }
        else ac}
      case None => new FeatureReturn
    }
  }
  
  def betweenPrevFn(fn: Fn)(s: Int, act_sarr:SourceSequence[Obs], pos:Int) =
    act_sarr.parentSeq match {
      case Some(sarr) =>
        if (pos > 0) {
        val curSt = act_sarr(pos).start
        val prevEnd = act_sarr(pos-1).end
        val pref = "BTWN_PREV_"
        ((prevEnd+1) to (curSt - 1)).foldLeft(new FeatureReturn) {(ac,p) => ac join (fn(s,sarr,p) updateS "BTWN_PREV_")}
        }
        else new FeatureReturn
      case None => new FeatureReturn
    }
  
  def binDistance(d: Int) = 
    if (d < 3) "L3"
    else if (d < 5) "L5"
    else if (d < 7) "L7"
    else "G6"
    
  def distancePrev(s: Int, act_sarr:SourceSequence[Obs], pos: Int) = 
	act_sarr.parentSeq match {
      case Some(sarr) =>
        if (pos > 0) {
          val curSt = act_sarr(pos).start
          val prevEnd = act_sarr(pos-1).end
          new FeatureReturn("prevDist-"+(binDistance(curSt-prevEnd)))
        } else new FeatureReturn
      case None => new FeatureReturn
      }
    
  def betweenSubsequentFn(fn: Fn)(s: Int, act_sarr:SourceSequence[Obs], pos:Int) =
    act_sarr.parentSeq match {
      case Some(sarr) =>
        if (pos < (act_sarr.length -1)) {
        val curEnd = act_sarr(pos).end
        val subSt = act_sarr(pos+1).start
        ((curEnd+1) to (subSt - 1)).foldLeft(new FeatureReturn) {(ac,p) => ac join (fn(s,sarr,p) updateS "BTWN_SUBSEQUENT_")}
         }
        else new FeatureReturn
      case None => new FeatureReturn
    }
  
  def distanceSubseq(s: Int, act_sarr:SourceSequence[Obs], pos: Int) = 
	act_sarr.parentSeq match {
      case Some(sarr) =>
        if (pos > (act_sarr.length - 1)) {
          val curEnd = act_sarr(pos).end
          val subSt = act_sarr(pos+1).start
          new FeatureReturn("subseqDist-"+(binDistance(subSt-curEnd)))
        } else new FeatureReturn
      case None => new FeatureReturn
    }

  override def ngramFn(fn:Fn, positions: Seq[Int], suc: Boolean)(s: Int, act_sarr:SourceSequence[Obs], pos:Int) = 
    act_sarr.parentSeq match {
      case Some(sarr) =>
        val st = act_sarr(pos).start
        val en = act_sarr(pos).end
        val maxLen = sarr.length
        val fname = new BuiltFeature
        var failed = false
        positions foreach {p =>
          val rp = if (p >= 0) en + p else st + p
          if ((rp >= 0) && (rp < maxLen)) {
        	val localPairs = fn(s,sarr,rp).features
        	if ((localPairs.size) < 1) failed = true
        	localPairs foreach {s => fname @@ s.toString; fname @@ '@'; fname @@ p.toString}}
          else if (rp == maxLen) fname @@ "-END-"
          else if (rp == -1) fname @@ "-START-" }
        if (failed) new FeatureReturn else new FeatureReturn(fname)
      case None =>
        // in this case, we're passing in the parent sequence, so call the super ngramFn that operates over the original sequence
        super.ngramFn(fn,positions,suc)(s, act_sarr, pos)
    }	    
}
