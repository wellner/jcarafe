package org.mitre.jcarafe.tokenizer

import org.mitre.jcarafe.lexer.{GenToker, Token}
import org.mitre.jcarafe.lexer.GenTokerConstants._
import collection.mutable.ListBuffer
import scala.annotation.tailrec

case class Matching(s: String, pos: Int)

class PatternSequence(var seq: List[PatternElement])
class SplitPatternSequence(seq: List[PatternElement]) extends PatternSequence(seq)
class MergePatternSequence(seq: List[PatternElement]) extends PatternSequence(seq)

abstract class PatternElement

sealed case class Category(val name: String, val set: Set[String]) extends PatternElement
case object Tk extends PatternElement
sealed case class R(val reg: util.matching.Regex) extends PatternElement
sealed case class S(val str: String) extends PatternElement
sealed case class Rep(val reg: util.matching.Regex) extends PatternElement
case object Recurse extends PatternElement 

abstract class TokenizerAugmenterPattern {
  def checkMatch(pat: PatternElement, s: String, kind: Int) : Boolean = pat match {
    case R(r1) => r1.findPrefixOf(s).isDefined
    case Category(_,cat) => cat.contains(s)
    case S(s1) => s1 == s
    case Tk => kind == TOK
    case Rep(r1) => r1.findPrefixOf(s).isDefined
    case Recurse => true
  }  
}

class SplitTokenizerAugmenterPattern(val patternSequences : List[SplitPatternSequence]) extends TokenizerAugmenterPattern {  
  
  def split(gstr: String, kind: Int) = {
    val slen = gstr.length
    var complete = false
    
    def applyPattern(patternSeq: List[PatternElement], str: String, slen: Int) : List[Element] = patternSeq match {
      case p1 :: S(s) :: ptail =>
        val pos = str.indexOf(s) // look ahead to find "s"
        if (pos > -1) {
          val sl = s.length
          val pre = str.substring(0,pos)
          val rest = str.substring(pos+sl,slen)
          if (checkMatch(p1,pre,kind)) {
            if (pos > 0)
              Tok(pre) :: Tok(s) :: applyPattern(ptail,rest,slen - pos - sl)
            else Tok(s) :: applyPattern(ptail,rest,slen - pos - sl)
          } else Nil
        } else Nil
      case Category(_,cat) :: R(r1) :: ptail =>  // case where we want to look ahead with Regex
        r1.findFirstMatchIn(str) match {
          case Some(m) =>
          	val pos = m.start
          	val pre = str.substring(0,pos)
          	if (cat.contains(pre)) {
              val rest = str.substring(m.end,slen)
          	  Tok(pre) :: Tok(m.toString) :: applyPattern(ptail,rest, slen - m.end)
          	} else Nil
          case None => Nil
        }
      case R(r1) :: Recurse :: Nil => // special case of greedy regexp matching to split 
        r1.findPrefixMatchOf(str) match {
          case Some(m) =>
            val rest = str.substring(m.end,slen)
            Tok(m.toString()) :: applyPattern((R(r1) :: Recurse :: Nil),rest,slen - m.end)
          case None => Nil
        }
      case R(r1) :: ptail =>
        r1.findPrefixMatchOf(str) match {
          case Some(m) =>
            val rest = str.substring(m.end,slen)
            Tok(m.toString()) :: applyPattern(ptail,rest,slen - m.end)
          case None => Nil
        }
      case Nil => complete = (slen == 0); Nil
      case _ => Nil
    }
    val toks = patternSequences.foldLeft(List(Tok(gstr)):List[Element]){case (ac,seq) => 
      complete = false
      val r = applyPattern(seq.seq,gstr,slen)
      if (complete && (r.length > ac.length)) r else ac
    }
    toks
  }
}

class MergeTokenizerAugmenterPattern(val patternSequences: Array[MergePatternSequence]) extends TokenizerAugmenterPattern {
  
  val numSeqs = patternSequences.size
  
  @tailrec
  final def mergePattern(pat: List[PatternElement], tokStream: List[Element], tokAcc:List[String]) : (Option[String], List[Element]) = {
    (pat,tokStream) match {
      case (Rep(rep) :: p2 :: ptail, t1 :: t2 :: r) =>
        if (checkMatch(Rep(rep), t1.getString, TOK)) {
          if (checkMatch(p2, t2.getString, TOK))
            mergePattern(ptail, r, t2.getString :: t1.getString :: tokAcc)
          else
            mergePattern(Rep(rep) :: p2 :: ptail, t2 :: r, t1.getString :: tokAcc)
        } else (None, tokStream)
      case (p1 :: ptail, t :: r) => // this case may be sufficient if there are not lookahead exceptions
        if (checkMatch(p1, t.getString, TOK))
          mergePattern(ptail, r, t.getString :: tokAcc)
        else (None, tokStream) 	
      case _ =>
        if (tokAcc.size > 0) {
          (Some(tokAcc.reverse.mkString), tokStream)
        }
        else (None, tokStream)	
    }
  }

  @tailrec
  final def merge(tokStream : List[Element], tbuf: ListBuffer[Element]) : Unit = {
    tokStream match {
      case tok :: tokTail =>
        var cont = true
        var nextToks : Option[List[Element]] = None
        var i = 0; while ((i < numSeqs) && cont) {
          mergePattern(patternSequences(i).seq,tokStream, Nil) match {
            case (Some(newTok),ntail) => 
              cont = false
              nextToks = Some(Tok(newTok) :: ntail)
            case _ => 
          }
          i += 1
        }
        nextToks match { 
          case Some(ns) => merge(ns,tbuf) 
          case None => tbuf append tok; merge(tokTail, tbuf)}
      case Nil => 
    }
  }
}

object SplitTokenizerAugmenterPattern {
  
  def apply(patternSeqs: List[PatternSequence]) = {
    val splitPatterns = 
      patternSeqs.foldLeft(List[SplitPatternSequence]()){ 
      case (ac,e) => e match {case e:SplitPatternSequence => e :: ac case _ => ac} }  
    new SplitTokenizerAugmenterPattern(splitPatterns.reverse)
  }
}

object MergeTokenizerAugmenterPattern {
  def apply(patternSeqs: List[PatternSequence]) = {
    val mergePatterns = 
      patternSeqs.foldLeft(List[MergePatternSequence]()){ 
      case (ac,e) => e match {case e:MergePatternSequence => e :: ac case _ => ac} }  
    new MergeTokenizerAugmenterPattern(mergePatterns.reverse.toArray)
  }
}
