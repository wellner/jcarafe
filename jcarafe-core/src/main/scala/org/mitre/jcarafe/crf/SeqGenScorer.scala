/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import org.mitre.jcarafe.util._

trait SeqGenScorer[Obs] extends DecodingSeqGen[Obs] {

  type Lab = AbstractLabel 
  val goldSets = new HashMap[AbstractLabel,Set[Annotation]] 
  val systemSets = new HashMap[AbstractLabel,Set[Annotation]]
  val goldTokens = new HashMap[AbstractLabel,Set[Int]]
  val systemTokens = new HashMap[AbstractLabel,Set[Int]]
  var globalIndex = 0

  var globalConfidenceCorrelation = List[(Double,Double,Double,Int,Double)]() // confidence, token score, tag score

  def trueLabel(a : AbstractLabel) : AbstractLabel = a match {
    case a: BeginState => a.unbegin
    case a: Any => a
  } 
  
  def isBegin(a: AbstractLabel) : Boolean = a match { case a : BeginState => true case a : Any => false } 
  
  def updateSets(sets: HashMap[AbstractLabel,Set[Annotation]], curT: AbstractLabel, st: Int, en: Int, gi: Int) : Unit = {
      val set = sets.get(curT) match {
        case Some(s) => s
        case None => 
          val s = new HashSet[Annotation]
          sets += (curT -> s)
          s
      }	
      set += new Annotation(st+gi,en+gi,false,curT,None)
    }
  
  override def getAccuracy : Double = {
    val of = new java.io.File(decodingOpts.evaluate match {case Some(f) => f case None => throw new RuntimeException("Invalid evaluation output file")})
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(of)), "UTF-8")
    var totalSharedPhrases = 0
    var totalGoldPhrases = 0
    var totalSysPhrases = 0
    val maxItemLen = goldSets.foldLeft(0){(ac,v) => if (v._1.toString.length > ac) v._1.toString.length else ac }
    os.write("%20s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n".format("","Prec","Rec","F1","Total","Shared","Missing","Spurious"))
    goldSets foreach {case(lab,set) =>
      if (!(lab.toString == "lex")) {
      val sysSet = systemSets.get(lab) match {case Some(s) => s case None => new HashSet[Annotation]}
      val totalGold = set.size 
      val totalSys = sysSet.size
      val sharedSet = sysSet.intersect(set) // create intersection
      val shared = sharedSet.size
      os.write("%20s".format(lab.toString))
      val p = if (totalSys > 0) (shared.toDouble / totalSys.toDouble) else 0.0
      val r = if (totalGold > 0) (shared.toDouble / totalGold.toDouble) else 0.0
      totalSharedPhrases += shared
      totalGoldPhrases += totalGold
      totalSysPhrases += totalSys
      val f = 2 * p * r / (p + r)
      os.write("\t%.3f\t%.3f\t%.3f\t%d\t%d\t%d\t%d\n".format(p,r,f,totalGold,shared,(totalGold-shared),(totalSys-shared)))
      }
    }
    val tp = if (totalSysPhrases > 0) totalSharedPhrases.toDouble / totalSysPhrases.toDouble else 0.0
    val tr = if (totalGoldPhrases > 0) totalSharedPhrases.toDouble / totalGoldPhrases.toDouble else 0.0
    val tf = 2 * tp * tr / (tp + tr) 
    os.write("%20s\t%.3f\t%.3f\t%.3f\t%d\t%d\t%d\t%d\n\n".format("TOTAL", tp, tr, tf, totalGoldPhrases, totalSharedPhrases, (totalGoldPhrases - totalSharedPhrases),(totalSysPhrases - totalSharedPhrases)))
    os.write("-------------- TOKEN LEVEL METRICS ---------------\n")
    var totalSharedTokens = 0
    var totalGoldTokens = 0
    var totalSysTokens = 0
    goldTokens foreach {case(lab,set) =>
      if (!(lab.toString == "lex")) {
      val sysSet = systemTokens.get(lab) match {case Some(s) => s case None => new HashSet[Int]}
      val totalGold = set.size 
      val totalSys = sysSet.size
      val sharedSet = sysSet.intersect(set) // create intersection
      val shared = sharedSet.size
      os.write("%20s".format(lab.toString))
      val p = if (totalSys > 0) (shared.toDouble / totalSys.toDouble) else 0.0
      val r = if (totalGold > 0) (shared.toDouble / totalGold.toDouble) else 0.0
      totalSharedTokens += shared
      totalGoldTokens += totalGold
      totalSysTokens += totalSys
      val f = 2 * p * r / (p + r)
      os.write("\t%.3f\t%.3f\t%.3f\t%d\t%d\t%d\t%d\n".format(p,r,f,totalGold,shared,(totalGold-shared),(totalSys-shared)))
      }
    }
    val tp_T = if (totalSysTokens > 0) totalSharedTokens.toDouble / totalSysTokens.toDouble else 0.0
    val tr_T = if (totalGoldTokens > 0) totalSharedTokens.toDouble / totalGoldTokens.toDouble else 0.0
    val tf_T = 2 * tp_T * tr_T / (tp_T + tr_T)
    os.write("\n%20s\t%.3f\t%.3f\t%.3f\t%d\t%d\t%d\t%d\n\n".format("TOTAL", tp_T, tr_T, tf_T, totalGoldTokens, totalSharedTokens, (totalGoldTokens - totalSharedTokens),(totalSysTokens - totalSharedTokens)))
    val acc = super.getAccuracy 
    os.write("\nToken Accuracy\t" + acc + "\n")
    if (decodingOpts.posteriors) {
      os.write("\n\nConfidence-Accuracy Table: \n\n");
      val rankings = getRankings(globalConfidenceCorrelation)
      rankings foreach {case (i,conf,tokA,tte,len,tagA) =>
        if (conf > 0.0) {
        os.write(i.toString)
        os.write('\t')
        os.write(len.toString)
        os.write('\t')
	    os.write(conf.toString)
        os.write('\t')
        os.write(tte.toString)
        os.write('\t')
        os.write(tokA.toString)
        os.write('\n')
        }
      }
    }
    os.close()
    acc
  }
  
  def getRankings(confTable: List[(Double,Double,Double,Int,Double)]) = {
    val ranks = (confTable.zipWithIndex map {case (v,i) => (v._1,i)}).sortWith({(p1,p2) => p1._1 > p2._1})
    val itable = confTable.toIndexedSeq
    ranks.zipWithIndex map {case ((_,r),i) => 
      val (s,t,tte,l,tg) = itable(r)
      (i,s,t,tte,l,tg)
    }
  }
  
  def updateTokenSet(sets: HashMap[AbstractLabel,Set[Int]], curT: AbstractLabel, pos: Int, gi: Int) : Unit = {
	  val set = sets.get(curT) match {
        case Some(s) => s
        case None => 
          val s = new HashSet[Int]
          sets += (curT -> s)
          s
      }	
      set += (pos + gi)
  }
  
  private def l2(x: Double) = math.log(x) / math.log(2.0)
  
  private def getEntropy(map: collection.mutable.Map[Int,Double]) = {
    var e = 0.0
    map foreach {case (i,v) => e -= l2(v) * v}
    e
  }
  
  override def evaluateSequences(seqs: Seq[InstanceSequence]) = {
    var startSysIndex = -1
    var curSysType = invLa(0)
    var startGoldIndex = -1
    var curGoldType = invLa(0)
    seqs foreach { s =>
      var seqTokIncorrect = 0
      var totalTokenEntropy = 0.0
      for (i <- 0 until s.length) {
        val inst = s.iseq(i)
        totalTokCnt += 1
        if (!(inst.label == inst.orig)) {totalIncorrectTok += 1; seqTokIncorrect += 1}
        val newSysAct = invLa(inst.label)
        val newGoldAct = invLa(inst.orig) 
        val newSysType = trueLabel(newSysAct) 
        val newGoldType = trueLabel(newGoldAct)
        updateTokenSet(systemTokens,newSysType,i,globalIndex)
        updateTokenSet(goldTokens,newGoldType,i,globalIndex)
        if (((i > 0) && !(newSysType == curSysType)) || (isBegin(newSysAct)) || (i == 0)) {
          if (i > 0) {
            updateSets(systemSets,curSysType,startSysIndex,i-1,globalIndex)
          }
          startSysIndex = i
          curSysType = newSysType
        }
        if (((i > 0) && !(newGoldType == curGoldType)) || (isBegin(newGoldAct)) || (i == 0)) {
          if (i > 0) {
            updateSets(goldSets,curGoldType,startGoldIndex,i-1,globalIndex)
          }
          startGoldIndex = i
          curGoldType = newGoldType
        }
        totalTokenEntropy += getEntropy(inst.condProbTbl)
      }
      if(s.length>0) {
        updateSets(systemSets,curSysType,startSysIndex,s.length-1,globalIndex)
        updateSets(goldSets,curGoldType,startGoldIndex,s.length-1,globalIndex)
      }
      globalIndex += s.length
      globalConfidenceCorrelation = (s.seqPosteriorProbability, (1.0 - (seqTokIncorrect.toDouble / s.length)), totalTokenEntropy, s.length, 0.0) :: globalConfidenceCorrelation
    }
  }	
}
