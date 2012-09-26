/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import scala.util.matching.Regex
import org.mitre.jcarafe.crf.{DynamicFeatureManager,SourceSequence,FeatureReturn,IncrementalMurmurHash}

//import net.didion.jwnl.data.IndexWord
//import net.didion.jwnl.data.Synset
//
//import net.didion.jwnl.data.POS

class PostFeatureManager(istring: String) extends DynamicFeatureManager[Array[String]](istring) {

  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  // redefine a simpleFn to include postFnExprs
  override def simpleFnExpr: Parser[FeatureFn] =
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | lexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | postFnExpr

  //def postFnExpr : Parser[FeatureFn] = lengthE | allWds | wordnetSynset | regexpUtteranceE | utterLexE | sameUserE | lexStartE
  def postFnExpr: Parser[FeatureFn] = lengthE | allWds | regexpUtteranceE | utterLexE | sameUserE | lexStartE | positionExpr | relPosExpr | allScores | allProps
  def lengthE: Parser[FeatureFn] = "lengthFn" ^^ { _ => lengthFn _ }
  def allWds: Parser[FeatureFn] = "allWordsFn" ^^ { _ => allWordsFn _ }
  def allScores: Parser[FeatureFn] = "allScoresFn" ^^ { _ => allScoresFn _ }
  def allProps: Parser[FeatureFn] = "allPropsFn" ^^ { _ => allPropsFn _ }
  def utterLexE: Parser[FeatureFn] = "utterLexFn" ^^ { _ => utterLexFn(false) _ }
  def sameUserE: Parser[FeatureFn] = "sameUserFn" ^^ { _ => sameUserFn _ }
  def lexStartE: Parser[FeatureFn] = "lexStartFn" ^^ { _ => lexStartFn _ }
  def positionExpr: Parser[FeatureFn] = "positionFn" ^^ { _ => posFn _ }

  def relPosExpr: Parser[FeatureFn] = "relPosFn" ^^ { _ => relPosFn _ }

  //def wordnetSynset : Parser[FeatureFn] = "wordnetFn" ^^ { _ => wordnetSynsetFn _}
  //def regexE : Parser[FeatureFn] = "

  def regexpUtteranceE: Parser[FeatureFn] = "regexpUtteranceFn(" ~> fname ~ "," ~ """[^)]+""".r <~ ")" ^^ { case (fn ~ _ ~ e) => _regexpUtteranceFn(fn, e.r) _ }

  def lengthFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    val l = act_sarr(pos).obs.length
    val lenCat = if (l < 3) "short" else if (l < 7) "medium" else if (l < 12) "long" else "verylong"
    new FeatureReturn("postLen=" + lenCat)
  }

  def relPosFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    val ln = act_sarr.length
    val fr1 =
      if (pos < 1) new FeatureReturn("posBegin")
      else if (pos < 2) new FeatureReturn("posBeg1")
      else if (pos < 4) new FeatureReturn("posBeg3")
      else if (pos < 6) new FeatureReturn("posBeg5") else new FeatureReturn
    if (ln < 3) fr1.updateS(":lnSm1:")
    else if (ln < 5) fr1.updateS(":lnSm2:")
    else if (ln < 7) fr1.updateS(":lnSm3:")
    else if (ln < 9) fr1.updateS(":lnSm4:")
    else if (ln < 15) fr1.updateS(":lnSm5:")
    else if (ln >= 15) fr1.updateS(":lnBig:")
    fr1
  }

  def posFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    val ln = act_sarr.length
    val eoff = ln - pos
    val fr1 =
      if (pos < 1) new FeatureReturn("posBegin")
      else if (pos < 2) new FeatureReturn("posBeg1")
      else if (pos < 4) new FeatureReturn("posBeg3")
      else if (pos < 6) new FeatureReturn("posBeg5") else new FeatureReturn
    val fr2 =
      if (eoff < 2) new FeatureReturn("posEnd")
      else if (eoff < 4) new FeatureReturn("posEnd3") else new FeatureReturn
    val fr3 = new FeatureReturn("posRaw", ((pos.toDouble + 1.0) / ln))
    fr1 join fr2 join fr3
  }

  def lexStartFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = lex match {
    case Some(l) =>
      val obs = act_sarr(pos).obs
      if (obs.length > 0) {
        l.get(obs(0)) match { case Some(r) => new FeatureReturn("lexStart:" + r) case None => new FeatureReturn }
      } else new FeatureReturn
    case None => new FeatureReturn
  }

  def utterLexFn(down: Boolean)(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = lex match {
    case Some(l) =>
      val ss = new scala.collection.mutable.ListBuffer[String]
      act_sarr(pos).obs.foreach { o =>
        val s = if (down) o.toLowerCase else o
        l.get(s) match { case Some(r) => ss += "lex:" + r case None => }
      }
      new FeatureReturn(ss.toList)
    case None => new FeatureReturn
  }

  def sameUserFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    if (pos > 0) {
      val u1 = act_sarr(pos).info match { case Some(m) => m.get("user") match { case Some(u) => u case None => "Unk" } case None => "Unk" }
      val u0 = act_sarr(pos - 1).info match { case Some(m) => m.get("user") match { case Some(u) => u case None => "Unk" } case None => "Unk" }
      if (u1 == u0) new FeatureReturn("SAME_USER") else new FeatureReturn("DIFFERENT_USER")
    } else new FeatureReturn
  }

  def joinList(list: List[String], joiner: String) = (list.head /: list.tail)(_ + joiner + _)

  def joinArray(array: Array[String], joiner: String): String = joinList(array.toList, joiner)

  def _regexpUtteranceFn(fname: String, regexp: Regex)(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    val fullString = act_sarr.generateOneLongString
    regexp.findFirstIn(fullString) match {
      case Some(_) => new FeatureReturn(fname)
      case None => new FeatureReturn
    }

    //val fullUtterance = joinArray(act_sarr, " ")
    // convert to a single string (adding whitspace separators)
  }

  //  def _regexpFn(fname:String, regexp:Regex)(s: Int, sarr: SourceSequence[Obs], pos: Int) = 
  //    regexp.findFirstIn(sarr(pos).obs.toString) match { 
  //      case Some(_) => new FeatureReturn(fname) 
  //      case None => new FeatureReturn}

  val numR = """[0-9]+""".r
  val punctR = """\p{Punct}+""".r
  val urlR = "http:".r
  private def collapseRepeatedChars(s: String) = {
    val sb = new StringBuilder
    var pc = '0'
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c != pc) sb += c
      pc = c
      i += 1
    }
    sb.toString
  }

  private def normalizeWord(w: String): String = {
    numR.findPrefixOf(w) match {
      case Some(v) => "NUM"
      case None =>
        punctR.findPrefixOf(w) match { case Some(p) => collapseRepeatedChars(p) case None => if (urlR.findPrefixOf(w).isDefined) "HTTP" else w.toLowerCase }
    }
  }

  def allWordsFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    new FeatureReturn(act_sarr(pos).obs.toList map normalizeWord)
  }

  private def getPrefixes(granularity: Int, str: String) = {
    val sb = new collection.mutable.ListBuffer[String]
    var i = 0
    val sl = str.length
    while (i < sl) {
      sb.append(str.substring(0, i))
      i += granularity
    }
    sb.append(str)
    sb.toList
  }

  def allPropsFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = {
    wdProps match {
      case Some(wp) =>
        act_sarr(pos).obs.toList.foldLeft(new FeatureReturn) {
          case (ac, w) => wp.get(IncrementalMurmurHash.hash(w)) match {
            case Some(c) =>
              ac join new FeatureReturn(c.flatMap { v => getPrefixes(4, v) })
            case None => ac
          }
        }
      case None => new FeatureReturn
    }
  }

  def allScoresFn(s: Int, act_sarr: SourceSequence[Array[String]], pos: Int) = wdScores match {
    case Some(l) =>
      val words = act_sarr(pos).obs.toSeq
      if (words.length > 0) {
        val scores: Seq[Double] = words map { w => l.get(hash(w)) match { case Some(v) => v case None => 0.0 } }
        val sumScores = scores.foldLeft(0.0) { _ + _ }
        val normScores = sumScores / words.length
        val maxScores = scores.max
        new FeatureReturn(":sumScores:", normScores) join new FeatureReturn(":maxScore:", maxScores)
      } else new FeatureReturn
    case None => new FeatureReturn
  }

  //  def wordnetSynsetFn(s: Int, act_sarr:SourceSequence[Array[String]], pos:Int) = {
  ////	val configFileStream = this.getClass().getClassLoader().getResourceAsStream("wordnet_java_file_properties.xml")
  ////    net.didion.jwnl.JWNL.initialize(configFileStream)
  ////    val dictionary = net.didion.jwnl.dictionary.Dictionary.getInstance()
  //    
  //
  ////    def lookupWord(currentWord : String) : List[String] = {
  ////	    val indexWord = dictionary.lookupIndexWord(POS.VERB, currentWord)
  ////        if (indexWord == null)
  ////        	{
  ////        		Nil
  ////            }
  ////        else
  ////            {
  ////			    val synsets = indexWord.getSenses()
  ////			    println("synsets:")
  ////			    println(synsets)
  ////			    println("END of synsets")
  ////			    
  ////			    val synsetArray =
  ////			        synsets map { "wn_synset" + _.getOffset() }
  ////			    synsetArray.toList
  ////			    //for (sense <- indexWord.getSenses()) yield { "wn_synset_" + sense.getOffset() }
  ////			}
  ////    }
  //    
  //    val returnedList = act_sarr(pos).obs.toList map 
  //      {
  //        currentWord =>  
  //	        // TODO what do I do about the wordnet requiring the POS (noun/verb/adjective/adverb)
  //            //println("about to call WordnetLookup.lookupWord()")
  //	        val lookupResult = WordnetLookup.lookupWord(currentWord)
  //            //println("lookupResults: " + lookupResult)
  //	        lookupResult
  //      }
  //    new FeatureReturn(List.flatten(returnedList), 1.0)
  //      
  //  }
  //    
  //}

  //object WordnetLookup
  //{
  //    private val dictionary =
  //      {
  //		val configFileStream = this.getClass().getClassLoader().getResourceAsStream("wordnet_java_file_properties.xml")
  //	    net.didion.jwnl.JWNL.initialize(configFileStream)
  //        net.didion.jwnl.dictionary.Dictionary.getInstance()
  //      }
  //    
  //
  //    def lookupWord(currentWord : String) : List[String] = {
  //        //println("beginning of lookupWord; about to call dictionary.lookupIndexWord...")
  //	    val indexWord = dictionary.lookupIndexWord(POS.VERB, currentWord)
  //        //println("after dictionary.lookupIndexWord.")
  //        if (indexWord == null)
  //        	{
  //        		Nil
  //            }
  //        else
  //            {
  //                //println("about to call indexWord.getSenses....")
  //			    val synsets = indexWord.getSenses()
  //			    //println("synsets:")
  //			    //println(synsets map {"synset: " + _.getOffset() + "; " })
  //		    	// synsets map { "wn_synset" + _.getOffset() }
  //			    //println("END of synsets")
  //			    
  //			    val synsetArray =
  //			        synsets map { "wn_synset" + _.getOffset() }
  //			    synsetArray.toList
  //			    //for (sense <- indexWord.getSenses()) yield { "wn_synset_" + sense.getOffset() }
  //			}
  //    }
}
 
