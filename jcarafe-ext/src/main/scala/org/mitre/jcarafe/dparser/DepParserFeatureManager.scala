/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.dparser
import org.mitre.jcarafe.crf.{ NonFactoredFeatureManager, FeatureReturn, ObsSource, SourceSequence, BuiltFeature, WordProperties }

abstract class DepParserFeatureManager(iString: String, var maxNumStates: Int) extends NonFactoredFeatureManager[String](iString) {
  import org.mitre.jcarafe.crf.IncrementalMurmurHash._

  val posPair_C = hash("posPair:")
  val wdPair_C = hash("wdPair:")
  val selfW_C = hash("SELFW:")
  val selfP_C = hash("SELFP:")
  val pPos_C = hash("pPos:")
  val pWord_C = hash("pWord:")
  val cPos_C = hash("cPos:")
  val cWord_C = hash("cWord:")
  val surround1_C = hash("surround1:")
  val surround2_C = hash("surround2:")
  val surround3_C = hash("surround3:")
  val surround4_C = hash("surround4:")
  val surround5_C = hash("surround5:")
  val surround6_C = hash("surround6:")
  val surround7_C = hash("surround7:")
  val surround8_C = hash("surround8:")
  val surround9_C = hash("surround9:")
  val surround10_C = hash("surround10:")
  val surround11_C = hash("surround11:")
  val surround12_C = hash("surround12:")
  val surround13_C = hash("surround13:")
  val surround14_C = hash("surround14:")
  val surround15_C = hash("surround15:")
  val endOfSeq_C = hash("END:")
  val startOfSeq_C = hash("START:")
  val inBetween_C = hash("inBetween:")
  val dist_C = hash("dist:")
  val dir_C  = hash("dir:")
  val cluster_C = hash("cluster:")
  val p_cluster_C = hash("parentCluster:")
  val rootPos     = hash("rootPos:")
  val rootToken   = hash("rootToken:")
  val pWordAndPos_C = mix(pWord_C, pPos_C)
  val cWordAndPos_C = mix(cWord_C, cPos_C)
  val pAndCPos_C = mix(pPos_C, cPos_C)
  val pAndCWord_C = mix(pWord_C, cWord_C)
  val pAndCCluster_C = mix(cluster_C, p_cluster_C)
  val pPosCWord_C = mix(pPos_C, cWord_C)
  val pWordCPos_C = mix(pWord_C, cPos_C)
  val pPosCWordCPos_C = mix(mix(pPos_C, cWord_C), cPos_C)
  val pWordPPosCWord_C = mix(pWordAndPos_C, cWord_C)
  val pWordPPosCPos_C = mix(pWordAndPos_C, cPos_C)
  val pWordCWordCPos_C = mix(pAndCWord_C, cPos_C)
  val backAll_C = hash("backALL_C:")
  val backParentPos_C = hash("backParPos_C:")
  val backCurrentPos_C = hash("backCurPos_C:")
  val backCPosOnly_C = hash("backCPosOnly_C:")
  val backPPosOnly_C = hash("backPPosOnly_C:")
  val backLexOnly_C = hash("backLexOnly_C:")
  val backC_C = hash("backC_C:")
  val backP_C = hash("backP_C:")
  val backC_Pos_C = hash("backC_PosC:")
  val backP_Pos_C = hash("backP_PosC:")

  def basicFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
    val srcCp = src(cp).code
    val curStr = src(cp).code
    val curPos = src(cp).posCode
    val ff = if (state == cp) (BuiltFeature(selfP_C) @@ curPos) else (BuiltFeature(posPair_C) @@ src(state).posCode @@ curPos)
    val gg = if (state == cp) {
      BuiltFeature(selfW_C) @@ curStr
    } else {
      BuiltFeature(wdPair_C) @@ src(state).obs.toString @@ curStr
    }
    new FeatureReturn(ff) join gg
  }

  def basicFns2(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
    if (cp > 0) {
      val prevPos1 = src(cp - 1).posCode
      val prev1Tok = src(cp - 1).obs.toString
      new FeatureReturn(BuiltFeature(pPos_C) @@ prevPos1) join (BuiltFeature(pWord_C) @@ prev1Tok)
    } else new FeatureReturn
  }

  private def getDistance(cp: Int, par: Int) = {    
    val ad = if (cp != par) math.abs(cp - par) else cp + 1 // handle case of root, (cp == par) 
    val dir = if (par <= cp) -1 else 1
    val ndist = 
      if (ad > 10) 11
      else if (ad > 5) 6
      else ad
    ndist * dir
  }

  private def getClusterCode(c: Long) = {
    wdProps match {
      case Some(wp) => wp.get(c) match { case Some(a :: _) => hash(a) case _ => 0L }
      case None => 0L
    }
  }

  def unigramFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val curStr = src(cp).code
      val curPos = src(cp).posCode
      val pStr = if (cp != state) src(state).code else rootToken
      val pPos = if (cp != state) src(state).posCode else rootPos
      val dd = getDistance(cp, state)
      val dir = if (state <= cp) -1 else 1
      //val cpCl = getClusterCode(src(cp).obs)  
      //val stateCl = getClusterCode(src(state).obs)
      
      val rr =
        (new FeatureReturn(BuiltFeature(pPos_C) @@ pPos))
          .join(BuiltFeature(pWord_C) @@ pStr)
          .join(BuiltFeature(cPos_C) @@ curPos)
          .join(BuiltFeature(cWord_C) @@ curStr)
          .join(BuiltFeature(pWordAndPos_C) @@ pStr @@ pPos)
          .join(BuiltFeature(cWordAndPos_C) @@ curStr @@ curPos)
          .join(BuiltFeature(pPos_C) @@ pPos @@ dist_C @@ dd)
          .join(BuiltFeature(pWord_C) @@ pStr @@ dist_C @@ dd)
          .join(BuiltFeature(cPos_C) @@ curPos @@ dist_C @@ dd)
          .join(BuiltFeature(cWord_C) @@ curStr @@ dist_C @@ dd)
          .join(BuiltFeature(pWordAndPos_C) @@ pStr @@ pPos @@ dist_C @@ dd)
          .join(BuiltFeature(cWordAndPos_C) @@ curStr @@ curPos @@ dist_C @@ dd)
          .join(BuiltFeature(pPos_C) @@ pPos @@ dir_C @@ dir)
          .join(BuiltFeature(pWord_C) @@ pStr @@ dir_C @@ dir)
          .join(BuiltFeature(cPos_C) @@ curPos @@ dir_C @@ dir)
          .join(BuiltFeature(cWord_C) @@ curStr @@ dir_C @@ dir)
          .join(BuiltFeature(pWordAndPos_C) @@ pStr @@ pPos @@ dir_C @@ dir)
          .join(BuiltFeature(cWordAndPos_C) @@ curStr @@ curPos @@ dir_C @@ dir)
      rr
    }

  def unigramPrefixFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val curStr = src(cp).code
      val pStr = if (cp != state) src(state).code else rootToken
      val pPos = if (cp != state) src(state).posCode else rootPos
      val cPos = src(cp).posCode
      val cPref = src(cp).prefCode
      val pPref = if (cp != state) src(state).prefCode else rootToken
      val dd = getDistance(cp, state)

      if (cPref != curStr || pStr != pPref) {
        val initial: FeatureReturn =
          new FeatureReturn(BuiltFeature(backLexOnly_C) @@ pPref @@ cPref)
            .join(BuiltFeature(backLexOnly_C) @@ pPref @@ cPref @@ dist_C @@ dd)
            .join(BuiltFeature(backAll_C) @@ pPref @@ pPos @@ cPref @@ cPos)
            .join(BuiltFeature(backAll_C) @@ pPref @@ pPos @@ cPref @@ cPos @@ dist_C @@ dd)
        val nxt1 =
          if (cPref != curStr) {
            initial
              .join(BuiltFeature(backCurrentPos_C) @@ pPos @@ cPref @@ cPos)
              .join(BuiltFeature(backPPosOnly_C) @@ pPos @@ cPref)
              .join(BuiltFeature(backCurrentPos_C) @@ pPos @@ cPref @@ cPos @@ dist_C @@ dd)
              .join(BuiltFeature(backPPosOnly_C) @@ pPos @@ cPref @@ dist_C @@ dd)
              .join(BuiltFeature(backC_Pos_C) @@ cPref @@ cPos)
              .join(BuiltFeature(backC_C) @@ cPref)
              .join(BuiltFeature(backC_Pos_C) @@ cPref @@ cPos @@ dist_C @@ dd)
              .join(BuiltFeature(backC_C) @@ cPref @@ dist_C @@ dd)
          } else initial
        if (pStr != pPref) {
          nxt1
            .join(BuiltFeature(backParentPos_C) @@ pPref @@ pPos @@ cPos)
            .join(BuiltFeature(backCPosOnly_C) @@ pPref @@ cPos)
            .join(BuiltFeature(backParentPos_C) @@ pPref @@ pPos @@ cPos @@ dist_C @@ dd)
            .join(BuiltFeature(backCPosOnly_C) @@ pPref @@ cPos @@ dist_C @@ dd)
            .join(BuiltFeature(backP_Pos_C) @@ pPref @@ pPos)
            .join(BuiltFeature(backP_C) @@ pPref)
            .join(BuiltFeature(backP_Pos_C) @@ pPref @@ pPos @@ dist_C @@ dd)
            .join(BuiltFeature(backP_C) @@ pPref @@ dist_C @@ dd)
        } else {
          nxt1
        }
      } else new FeatureReturn
    }

  def bigramFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val curStr = src(cp).code
      val curPos = src(cp).posCode
      val pStr = if (cp != state) src(state).code else rootToken
      val pPos = if (cp != state) src(state).posCode else rootPos
      val dd = getDistance(cp, state)
      //val cpCl = getClusterCode(src(cp).obs)
      val stateCl = getClusterCode(src(state).code)
      new FeatureReturn(BuiltFeature(pAndCPos_C) @@ pPos @@ curPos)
        .join(BuiltFeature(pAndCWord_C) @@ pStr @@ curStr)
        //.join (BuiltFeature(pAndCCluster_C) @@ cpCl @@ stateCl)
        .join(BuiltFeature(pWordPPosCPos_C) @@ pStr @@ pPos @@ curPos)
        .join(BuiltFeature(pPosCWordCPos_C) @@ pPos @@ curStr @@ curPos)
        .join(BuiltFeature(pPosCWord_C) @@ pPos @@ curStr)
        .join(BuiltFeature(pWordCPos_C) @@ pStr @@ curPos)
        .join(BuiltFeature(pAndCPos_C) @@ pAndCWord_C @@ pStr @@ pPos @@ curStr @@ curPos)
        //.join (BuiltFeature(pPosCWordCPos_C) @@ pPos @@ curStr @@ curPos @@ cpCl @@ stateCl)
        .join(BuiltFeature(pAndCPos_C) @@ pPos @@ curPos @@ dist_C @@ dd)
        .join(BuiltFeature(pAndCWord_C) @@ pStr @@ curStr @@ dist_C @@ dd)
        .join(BuiltFeature(pWordPPosCPos_C) @@ pStr @@ pPos @@ curPos @@ dist_C @@ dd)
        .join(BuiltFeature(pPosCWordCPos_C) @@ pPos @@ curStr @@ curPos @@ dist_C @@ dd)
        .join(BuiltFeature(pPosCWord_C) @@ pPos @@ curStr @@ dist_C @@ dd)
        .join(BuiltFeature(pWordCPos_C) @@ pStr @@ curPos @@ dist_C @@ dd)
        .join(BuiltFeature(pAndCPos_C) @@ pAndCWord_C @@ pStr @@ pPos @@ curStr @@ curPos @@ dist_C @@ dd)
    }

  def prefixFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
    val curStr = src(cp).obs.toString
    new FeatureReturn
  }

  def inBetweenFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val srcCp = src(cp)
      val curPos = src(cp).posCode
      val curCoarsePos = src(cp).cPosCode
      val pPos = if (cp != state) src(state).posCode else rootPos
      val pCoarsePos = if (cp != state) src(state).cPosCode else rootPos
      val dir = if (state <= cp) -1 else 1
      val st = if (cp != state) math.min(state, cp) else -1
      val en = math.max(state, cp)
      var fr = new FeatureReturn
      val dd = getDistance(cp, state)
      var i = st + 1
      val left = if (dir <= 0) pPos else curPos
      val right = if (dir <= 0) curPos else pPos
      val leftC = if (dir <= 0) pCoarsePos else curCoarsePos
      val rightC = if (dir <= 0) curCoarsePos else pCoarsePos
      while (i < en) {
        val midC = src(i).cPosCode
        val mid = src(i).posCode
        fr = 
          fr.join(BuiltFeature(inBetween_C) @@ left @@ mid @@ right)
        	.join(BuiltFeature(inBetween_C) @@ left @@ mid @@ right @@ dd)
        	.join(BuiltFeature(inBetween_C) @@ left @@ mid @@ right @@ dir_C @@ dir)
        	.join(BuiltFeature(inBetween_C) @@ leftC @@ midC @@ rightC)
        	.join(BuiltFeature(inBetween_C) @@ leftC @@ midC @@ rightC @@ dd)
        	.join(BuiltFeature(inBetween_C) @@ leftC @@ midC @@ rightC @@ dir_C @@ dir)
        i += 1
      }
      fr
    }
    
  private def getSurroundingFeatures(cPos: Long, cPosPlus1: Long, cPosMinus1: Long, pPos: Long, pPosPlus1: Long, pPosMinus1: Long, dd: Int) = {
    val dir = if (dd <= 0) -1 else 1
    new FeatureReturn(BuiltFeature(surround1_C) @@ cPosMinus1 @@ cPos @@ pPos @@ pPosPlus1)
        .join(BuiltFeature(surround2_C) @@ cPos @@ pPos @@ pPosPlus1)
        .join(BuiltFeature(surround3_C) @@ cPosMinus1 @@ cPos @@ pPos)
        .join(BuiltFeature(surround4_C) @@ cPosMinus1 @@ pPos @@ pPosPlus1)
        .join(BuiltFeature(surround5_C) @@ cPosMinus1 @@ cPos @@ pPosPlus1)
        //.join(BuiltFeature(surround1_C) @@ cPosMinus1 @@ cPos @@ pPos @@ pPosPlus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround2_C) @@ cPos @@ pPos @@ pPosPlus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround3_C) @@ cPosMinus1 @@ cPos @@ pPos @@ dist_C @@ dd)
        //.join(BuiltFeature(surround4_C) @@ cPosMinus1 @@ pPos @@ pPosPlus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround5_C) @@ cPosMinus1 @@ cPos @@ pPosPlus1 @@ dist_C @@ dd)
        .join(BuiltFeature(surround1_C) @@ cPosMinus1 @@ cPos @@ pPos @@ pPosPlus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround2_C) @@ cPos @@ pPos @@ pPosPlus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround3_C) @@ cPosMinus1 @@ cPos @@ pPos @@ dir_C @@ dir)
        .join(BuiltFeature(surround4_C) @@ cPosMinus1 @@ pPos @@ pPosPlus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround5_C) @@ cPosMinus1 @@ cPos @@ pPosPlus1 @@ dir_C @@ dir)
        // Now handle for cPosPlus1 and pPosMinus1
        .join(BuiltFeature(surround6_C) @@ cPosPlus1 @@ cPos @@ pPos @@ pPosMinus1)
        .join(BuiltFeature(surround7_C) @@ cPos @@ pPos @@ pPosMinus1)
        .join(BuiltFeature(surround8_C) @@ cPosPlus1 @@ cPos @@ pPos)
        .join(BuiltFeature(surround9_C) @@ cPosPlus1 @@ pPos @@ pPosMinus1)
        .join(BuiltFeature(surround10_C) @@ cPosPlus1 @@ cPos @@ pPosMinus1)
        //.join(BuiltFeature(surround6_C) @@ cPosPlus1 @@ cPos @@ pPos @@ pPosMinus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround7_C) @@ cPos @@ pPos @@ pPosMinus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround8_C) @@ cPosPlus1 @@ cPos @@ pPos @@ dist_C @@ dd)
        //.join(BuiltFeature(surround9_C) @@ cPosPlus1 @@ pPos @@ pPosMinus1 @@ dist_C @@ dd)
        //.join(BuiltFeature(surround10_C) @@ cPosPlus1 @@ cPos @@ pPosMinus1 @@ dist_C @@ dd)
        .join(BuiltFeature(surround6_C) @@ cPosPlus1 @@ cPos @@ pPos @@ pPosMinus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround7_C) @@ cPos @@ pPos @@ pPosMinus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround8_C) @@ cPosPlus1 @@ cPos @@ pPos @@ dir_C @@ dir)
        .join(BuiltFeature(surround9_C) @@ cPosPlus1 @@ pPos @@ pPosMinus1 @@ dir_C @@ dir)
        .join(BuiltFeature(surround10_C) @@ cPosPlus1 @@ cPos @@ pPosMinus1 @@ dir_C @@ dir)
        // Now for cPosPlus1 and pPosPlus1 and cPosMinus1 and pPosMinus1 - the backoffs for these are covered by the above
        .join(BuiltFeature(surround11_C) @@ cPosMinus1 @@ cPos @@ pPosMinus1 @@ pPos)
        //.join(BuiltFeature(surround11_C) @@ cPosMinus1 @@ cPos @@ pPosMinus1 @@ pPos @@ dist_C @@ dd)
        .join(BuiltFeature(surround11_C) @@ cPosMinus1 @@ cPos @@ pPosMinus1 @@ pPos @@ dir_C @@ dir)
        .join(BuiltFeature(surround12_C) @@ cPosPlus1 @@ cPos @@ pPosPlus1 @@ pPos)
        //.join(BuiltFeature(surround12_C) @@ cPosPlus1 @@ cPos @@ pPosPlus1 @@ pPos @@ dist_C @@ dd)
        .join(BuiltFeature(surround12_C) @@ cPosPlus1 @@ cPos @@ pPosPlus1 @@ pPos @@ dir_C @@ dir)
    }
        
   def surroundingWordCoarsePosFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val ln = src.length
      val cPos = src(cp).cPosCode
      val cPosPlus1 = if (cp < ln - 1) src(cp + 1).cPosCode else endOfSeq_C
      val cPosMinus1 = if (cp > 0) src(cp - 1).cPosCode else startOfSeq_C
      val pPos = if (cp != state) src(state).cPosCode else rootPos
      val pPosPlus1 = if (cp != state) (if (state < ln - 1) src(state + 1).cPosCode else endOfSeq_C) else src(0).cPosCode 
      val pPosMinus1 = if (cp != state) (if (state > 0) src(state - 1).cPosCode else startOfSeq_C) else startOfSeq_C
      val dd = getDistance(cp, state)
      getSurroundingFeatures(cPos, cPosPlus1, cPosMinus1, pPos, pPosPlus1, pPosMinus1,dd)
    }
  
  def surroundingWordPosFns(ss: Int, src: SourceSequence[String], cp: Int, state: Int): FeatureReturn = {
      val ln = src.length
      val cPos = src(cp).posCode
      val cPosPlus1 = if (cp < ln - 1) src(cp + 1).posCode else endOfSeq_C
      val cPosMinus1 = if (cp > 0) src(cp - 1).posCode else startOfSeq_C
      val pPos = if (cp != state) src(state).posCode else rootPos
      val pPosPlus1 = if (cp != state) (if (state < ln - 1) src(state + 1).posCode else endOfSeq_C) else src(0).cPosCode
      val pPosMinus1 = if (cp != state) (if (state > 0) src(state - 1).posCode else startOfSeq_C) else startOfSeq_C
      val dd = getDistance(cp, state)
      getSurroundingFeatures(cPos, cPosPlus1, cPosMinus1, pPos, pPosPlus1, pPosMinus1, dd)
    }

  // these need to be added "by hand" in the code and can not be specified through a config file
  // change this in the future
  val fnList = Nil

}
