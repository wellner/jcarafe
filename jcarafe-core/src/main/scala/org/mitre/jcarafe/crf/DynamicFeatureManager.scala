/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import org.mitre.jcarafe.util._

/*
 * A DynamicFeatureManager is able to load feature extraction functions
 * at <i>runtime</i> by reading from a feature extraction specification file.
 * This allows the same piece of software to accommodate very different tagging
 * and sequence labeling tasks.  Crucially, it allows users to customize
 * feature extraction functionality without needing to recompile.
 * The initialize() method must be called to create the feature extraction functions
 * from the provided spec. 
 * 
 * @author Ben Wellner
*/
class DynamicFeatureManagerBuilder[Obs](
    lex: Option[BloomLexicon], 
    wdProps: Option[WordProperties],
    wdScores: Option[WordScores],
    inducedFeatureMap: Option[InducedFeatureMap],
    iString: String,
    inducingNewMap: Boolean = false) extends FeatureManagerBuilder[Obs](lex, wdProps, wdScores, inducedFeatureMap, iString, inducingNewMap) with RegexParsers {
  
  def this(s: String) = this(None, None, None, None, s)

  // implicitly map function objects to FeatureFn objects
  implicit def upgrad(f: ((Int, SourceSequence[Obs], Int) => FeatureReturn)) : FeatureFn[Obs] = { FeatureFn(f) }
  
  var ii = 0
  def genSym = {
    val r = "f_"+ii
    ii += 1
    r
  }
  
  def buildFeatureFns(s: String = "default") = parseString(iString)  
  
  def topExprs : Parser[List[FeatureFn[Obs]]] = repsep(topExpr,"""[;\r\n]+""".r) ~ ";" ^^ {case (e~_) => e filter {_.isDefined} map {_.get}}
  
  def topExpr : Parser[Option[FeatureFn[Obs]]] = topExpr0 | topExpr1 | topExpr2
  
  def topExpr0 : Parser[Option[FeatureFn[Obs]]] = comment ^^ {_ => None}

  def topExpr1 : Parser[Option[FeatureFn[Obs]]] = 
    fname ~ ("as" | ":") ~ fullExpr ~ opt("TRANSITION"|"NEURAL"|"MULTI") ~ opt(comment) ^^ 
    {case (l~_~f~t~_) => 
      val fFun = t match {case Some("TRANSITION") => FeatureFn(l,f,true) 
				case Some("NEURAL") => FeatureFn(l,f,false,NNFeature) 
				case Some("MULTI") => FeatureFn(l,f,false,MultiFeature)
				case _ => FeatureFn(l,f,false)}
      Some(fFun)
      }
  
  def topExpr2 : Parser[Option[FeatureFn[Obs]]] = 
    fullExpr ~ opt("TRANSITION"|"NEURAL"|"MULTI") ~ opt(comment) ^^ 
    {case (f~t~_) =>
      val l = genSym
      val fFun = t match {
      	case Some("TRANSITION") => FeatureFn[Obs](l,f,true) 
      	case Some("NEURAL") => FeatureFn[Obs](l,f,false,NNFeature) 
      	case Some("MULTI") => FeatureFn[Obs](l,f,false,MultiFeature)
      	case _ => FeatureFn[Obs](l,f,false)}
      Some(fFun)
      }  
  
  def comment : Parser[String] = """#.*""".r

  def fname : Parser[String] = """[A-z0-9_\.:-]+""".r
  
  def fullExpr : Parser[FeatureFn[Obs]] = 
    (cfnExpr ~ ("cross"|"X") ~ fullExpr) ^^ {case (l~_~r) => l.cross(r)} |
    (simpleFnExpr ~ ("cross"|"X") ~ fullExpr) ^^ {case (l~_~r) => l.cross(r)} | 
    ("(" ~ cfnExpr ~ ")" ~ ("cross"|"X") ~ "(" ~ fullExpr ~ ")") ^^ {case (_~l~_~_~_~r~_) => l.cross(r)} |
    ("(" ~ simpleFnExpr ~ ")" ~ ("cross"|"X") ~ "(" ~ fullExpr ~ ")") ^^ {case (_~l~_~_~_~r~_) => l.cross(r)} | 
    cfnExpr
  
  def cfnExpr : Parser[FeatureFn[Obs]] =  
    (simpleFnExpr ~ ("ngram"|"N-") ~ rangeExpr ~ opt("SUCCEED")) ^^ {case (l~_~r~o) => l.ngram(o,r:_*)} |
    (simpleFnExpr ~ ("within"|"W") ~ rangeExpr) ^^ {case (l~_~r) => l.within(r:_*)} |
    (simpleFnExpr ~ ("over"|"@") ~ rangeExpr)   ^^ {case (l~_~r) => l.over(r:_*)} |
    (simpleFnExpr ~ ("self"|"<>") ~ fname ~ opt(rangeExpr)) ^^ {case (l~_~r~s) => s match {case None => l.self(r) 
											   case Some(s) => l.self(r,s:_*)}} |
    ("(" ~ fullExpr ~ ")" ~ ("ngram"|"N-") ~ rangeExpr ~ opt("SUCCEED")) ^^ {case (_~l~_~_~r~o) => l.ngram(o,r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("within"|"W-") ~ rangeExpr) ^^ {case (_~l~_~_~r) => l.within(r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("over"|"@-") ~ rangeExpr) ^^ {case (_~l~_~_~r) => l.over(r:_*)} |
    ("(" ~ fullExpr ~ ")" ~ ("displaced") ~ capStringExpr) ^^ { case (_~l~_~_~posTags) => l.displaced(posTags) } |
    ("(" ~ fullExpr ~ ")" ~ ("displaced")) ^^ { case (_~l~_~_) => l.displaced } |
    ("(" ~ fullExpr ~ ")" ~ ("self"|"<>") ~ fname ~ opt(rangeExpr)) ^^ { case (_~l~_~_~r~s) => s match {case None => l.self(r) 
												      case Some(s) => l.self(r,s:_*) }} |
    simpleFnExpr
  
  def simpleFnExpr : Parser[FeatureFn[Obs]] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | caseLessFnExpr | lexFnExpr | wdPropFnExpr | downWdPropFnExpr | wdScoreFnExpr |
    downLexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | attributeFnExpr |
    weightedAttrExpr | distToLeftExpr | distToRightExpr | nodeFnSemiExpr | edgeFnSemiExpr | phraseFnExpr | semiAttributeFnExpr | phraseWdsExpr |
    prefNGramExpr | sufNGramExpr | sentPosExpr | wdLenExpr | wdFnNormExpr | wdPropPrefixFnExpr | posFnExpr 
  
  def wdFnExpr : Parser[FeatureFn[Obs]]           = "wdFn" ^^ {_ => wdFn}
  def wdFnNormExpr : Parser[FeatureFn[Obs]]       = "wdNormFn" ^^ {_ => wdFnNorm _}
  def caseLessFnExpr : Parser[FeatureFn[Obs]]     = "caselessWdFn" ^^ {_ => caselessWdFn}
  def posFnExpr: Parser[FeatureFn[Obs]]           = "preLabFn" ^^ {_ => _preLabFn _}
  def lexFnExpr : Parser[FeatureFn[Obs]]          = "lexFn" ^^ {_ => lexFn}
  def downLexFnExpr : Parser[FeatureFn[Obs]]      = "downLexFn" ^^ {_ => downLexFn}
  def wdPropFnExpr : Parser[FeatureFn[Obs]]       = "wdPropFn" ^^ {_ => wordPropertiesFn(false) _ }
  def wdPropPrefixFnExpr : Parser[FeatureFn[Obs]] = "wdPropPrefixFn(" ~> intVal <~ ")" ^^ {v => wordPropertiesPrefixesFn(v,false) _ }
  def downWdPropFnExpr : Parser[FeatureFn[Obs]]   = "downWdPropFn" ^^ {_ => wordPropertiesFn(true) _ }
  def wdScoreFnExpr : Parser[FeatureFn[Obs]]      = "wdScoreFn" ^^ {_ => wordScoresFn _ }
  def nodeFnExpr : Parser[FeatureFn[Obs]]         = "nodeFn" ^^ {_ => nodeFn}
  def nodeFnSemiExpr : Parser[FeatureFn[Obs]]     = "semiNodeFn" ^^ {_ => _nodeFnSemi _}
  def edgeFnSemiExpr : Parser[FeatureFn[Obs]]     = "semiEdgeFn" ^^ {_ => _edgeFnSemi _}
  def semiAttributeFnExpr: Parser[FeatureFn[Obs]] = "semiAttrFn(" ~> fname <~ ")" ^^ {v => _phraseAttributeFn(v) _}
  def phraseFnExpr : Parser[FeatureFn[Obs]]       = "phraseFn" ^^ {_ => phraseFn _}
  def phraseWdsExpr : Parser[FeatureFn[Obs]]      = "phraseWds" ^^ {_ => phraseWds _}
  def edgeFnExpr : Parser[FeatureFn[Obs]]         = "edgeFn" ^^ {_ => edgeFn}
  def allTagFnExpr : Parser[FeatureFn[Obs]]       = "allTagFn" ^^ {_ => allTagFn _}
  def weightedAttrExpr : Parser[FeatureFn[Obs]]   = "weightedAttrs" ^^ {_ => weightedAttributes _}
  def distToLeftExpr : Parser[FeatureFn[Obs]]     = "distToLeft(" ~> fname ~ "," ~ fname <~ ")" ^^ {case (a~_~v) => distanceToLeft(a,v) _ }
  def distToRightExpr : Parser[FeatureFn[Obs]]    = "distToRight(" ~> fname ~ "," ~ fname <~ ")" ^^ {case (a~_~v) => distanceToRight(a,v) _ }
  def sentPosExpr : Parser[FeatureFn[Obs]]        = "sentPos" ^^ {_ => sentPosition _}
  
  def prefFnExpr : Parser[FeatureFn[Obs]]         = "prefixFn(" ~> intVal <~ ")" ^^ { v => prefixFn(v) _}

  def attributeFnExpr : Parser[FeatureFn[Obs]]    = "attributeFn(" ~> fname <~ ")" ^^ {v => attributeFn(v) _}
  def sufFnExpr : Parser[FeatureFn[Obs]]          = "suffixFn(" ~> intVal <~ ")" ^^ { v => suffixFn(v) _}
  def antiPrefFnExpr : Parser[FeatureFn[Obs]]     = "antiPrefixFn(" ~> intVal <~ ")" ^^ { v => antiPrefixFn(v) _}
  def antiSufFnExpr : Parser[FeatureFn[Obs]]      = "antiSuffixFn(" ~> intVal <~ ")" ^^ { v => antiSuffixFn(v) _}
  def prefNGramExpr : Parser[FeatureFn[Obs]]      = "prefNGrams(" ~> intVal ~ "," ~ intVal <~ ")" ^^ {case (s~_~r) => prefNgrams(s,r) _ }
  def sufNGramExpr : Parser[FeatureFn[Obs]]       = "sufNGrams(" ~> intVal ~ "," ~ intVal <~ ")" ^^ {case (s~_~r) => sufNgrams(s,r) _ }
  def wdLenExpr : Parser[FeatureFn[Obs]]          = "wdLen" ^^ {_ => wdLen _}

  def regexpFnExpr : Parser[FeatureFn[Obs]]       = "regexpFn(" ~> fname ~ "," ~ """(\\\)|[^\)])+""".r <~ ")" ^^ 
                                            {case (fn~_~e) => 
					      val parenMap = """\\\)""".r // use to map escaped parens to actual parens
					      val ne = parenMap.replaceAllIn(e,{_ => ")"})
					      FeatureFn(_regexpFn(fn,ne.r) _)}
  def predicateExpr : Parser[FeatureFn[Obs]]      = "predicateFn(" ~ fname ~ "," ~ repsep(simpleFnExpr,",") ~ ")" ^^ {case (_~fn~_~fs~_) => predicateFn(fn,fs) _}
  
  def rangeExpr : Parser[Seq[Int]] = basRangeExpr | toRangeExpr
  def basRangeExpr : Parser[Seq[Int]] = "(" ~> repsep(intVal,",") <~ ")" ^^ {_.toSeq}
  def toRangeExpr : Parser[Seq[Int]] = "(" ~> intVal ~ "to" ~ intVal <~ ")" ^^ {case(f~_~t) => (f to t).toSeq} 
  def capStringExpr : Parser[Seq[String]] = "(" ~> repsep(capStringVal, ",") <~ ")" ^^ {_.toSeq}
  def intVal : Parser[Int] = """-?[0-9]+""".r ^^ {_.toInt}
  def trueVal : Parser[Boolean] = "true" ^^ {_ => true}
  def falseVal : Parser[Boolean] = "false" ^^ {_ => false}
  def boolVal : Parser[Boolean] = trueVal | falseVal
  def capStringVal : Parser[String] = """[A-Z]+""".r ^^ {a => a}

  def parseIt(r: java.io.Reader) : List[FeatureFn[Obs]] = { 
    val res = parseAll(topExprs,new PagedSeqReader(PagedSeq.fromReader(r)))
    if (res.successful) res.get else throw new RuntimeException("Feature Specification File failed to Parse") }
  def parseString(s: String) : List[FeatureFn[Obs]] = parseIt(new java.io.StringReader(s))
  
}

/** 
 * This dynamic feature manager redefines window and ngram functions to 
 * operate over the original sequence of elements in the context where 
 * we've recoded the original sequence to a selected sub-sequence with
 * re-mapped labels.
 * <i>Design note: This could be a trait also</i>
 */
class DynamicRecodeFeatureManager[Obs](iString: String) extends DynamicFeatureManagerBuilder[Obs](None,None,None,None,iString) {
  
  override def simpleFnExpr : Parser[FeatureFn[Obs]] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | lexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | recodeFnExpr
  
  def recodeFnExpr : Parser[FeatureFn[Obs]] = betweenPrevExpr | betweenSubExpr | distancePrevious | distanceSubsequent 
  
  def betweenPrevExpr : Parser[FeatureFn[Obs]] = "betweenPrevious(" ~> cfnExpr <~ ")" ^^ {fn => FeatureFn(betweenPrevFn(fn) _)}
  def betweenSubExpr : Parser[FeatureFn[Obs]] = "betweenSubsequent(" ~> cfnExpr <~ ")" ^^ {fn => FeatureFn(betweenSubsequentFn(fn) _)}
  def distancePrevious : Parser[FeatureFn[Obs]] = "distancePrevious" ^^ { _ => FeatureFn(distancePrev _)}
  def distanceSubsequent : Parser[FeatureFn[Obs]] = "distanceSubsequent" ^^ { _ => FeatureFn(distancePrev _)}
    
  def windowFn(fn:FeatureFn[Obs], window:Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = 
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
  def windowAnyFn(fn:FeatureFn[Obs], window:Seq[Int])(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
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
  
  def betweenPrevFn(fn: FeatureFn[Obs])(s: Int, act_sarr:SourceSequence[Obs], pos:Int) =
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
    
  def betweenSubsequentFn(fn: FeatureFn[Obs])(s: Int, act_sarr:SourceSequence[Obs], pos:Int) =
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

 	    
}
