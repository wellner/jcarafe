package org.mitre.jcarafe.scopetagger

import org.mitre.jcarafe.crf.DynamicFeatureManagerBuilder
import org.mitre.jcarafe.crf.SourceSequence
import org.mitre.jcarafe.crf.{FeatureReturn, FeatureFn}

class ScopeFeatureManagerBuilder(iString: String) extends DynamicFeatureManagerBuilder[String](iString) {
	  
  override def simpleFnExpr : Parser[FeatureFn[String]] = 
    	predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | caseLessFnExpr | lexFnExpr | 
    	downLexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | attributeFnExpr |
    	distToLeftExpr | distToRightExpr | scopeFnExpr
    
  def scopeFnExpr : Parser[FeatureFn[String]] = relPos | intervening 
  def relPos : Parser[FeatureFn[String]] = "relPos" ^^ {_ => relativePosition _ }
  def intervening : Parser[FeatureFn[String]] = "interLex" ^^ {_ => interveningLex _}

  def directionDistance(cur: Int, tar: Int) : (String,String) = {
    val diff = tar - cur
    val dist = scala.math.abs(diff)
    val dStr = 
      if (dist < 1) "0" 
      else if (dist < 1) "1" 
      else if (dist < 2) "2" 
      else if (dist < 4) "4" 
      else if (dist < 6) "6" 
      else if (dist < 11) "11" 
      else if (dist < 15) "15" 
      else "16+" 
    val dir = if (diff >= 0) "Left" else "Right"
    (dir,dStr) 
  }
	
  def interveningLex(s: Int, act_sarr:SourceSequence[String], pos:Int) = lex match {
    case Some(lex) =>
      (act_sarr.getInfo("cueType"), act_sarr.getInfo("cueStartPos")) match {
	case (Some(t),Some(tarPos)) =>
	  val tp = tarPos.toInt
	  val (dir,dist) = directionDistance(pos,tp)
	  val lb = new scala.collection.mutable.HashSet[String]
	  var i = pos
	  val inc = if ((tp - pos) > 0) 1 else -1 
	  while (i != tp) {
	    val s = act_sarr(i).obs.toString
	    lex.get(s) match {case Some(lt) => lb += ("inter_"+lt) case None => }
	    i += inc
	  }
	new FeatureReturn(lb.toList)
	case _ => new FeatureReturn}
    case None => new FeatureReturn
  }
		
	
  def relativePosition(s: Int, act_sarr:SourceSequence[String], pos:Int) = 
    (act_sarr.getInfo("cueType"), act_sarr.getInfo("cueStartPos")) match {
      case (Some(t),Some(tarPos)) => 
	val (dir,dist) = directionDistance(pos,tarPos.toInt)
        new FeatureReturn(List("DD="+dir+dist, "Dir="+dir))
      case _ => new FeatureReturn
    }
}
