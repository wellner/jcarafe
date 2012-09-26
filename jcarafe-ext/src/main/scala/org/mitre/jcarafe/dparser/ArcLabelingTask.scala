package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{StdTrainer,StdDecoder,TrainingSeqGen,FactoredDecodingSeqGen,StdModel,
  StandardSerializer,DynamicFeatureManager,TrainingFactoredFeatureRep,DecodingFactoredFeatureRep,SourceSequence,FeatureReturn,IncrementalMurmurHash,
  BuiltFeature,FeatureManager}
import org.mitre.jcarafe.util.Options
import org.mitre.jcarafe.tagger.TaggerTask

class ArcLabeler(argv: Array[String]) extends TaggerTask[String](argv) {
  
  lazy val trainer = new StdTrainer(opts) {
    val mgr = new ArcLabelingFeatureManager[String](FeatureManager.getFeatureSpecString(opts.featureSpec.get))
    val fr = new TrainingFactoredFeatureRep[String](mgr,opts)
    val sGen = new TrainingSeqGen[String](fr,opts) with CoNLLSeqGenLabel
  } 
  
  lazy val decoder = new StdDecoder(opts) {
    val mgr = new ArcLabelingFeatureManager[String](opts.featureSpec.get)
    val fr = new DecodingFactoredFeatureRep[String](mgr,opts,model)
    val sGen = new FactoredDecodingSeqGen[String](fr,model,opts) with CoNLLSeqGenLabel
  }

}

class ArcLabelingFeatureManager[Obs](iString: String) extends DynamicFeatureManager[Obs](iString) {
  
  override def simpleFnExpr : Parser[FeatureFn] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | caseLessFnExpr | lexFnExpr | wdPropFnExpr | downWdPropFnExpr | wdScoreFnExpr |
    downLexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | attributeFnExpr |
    weightedAttrExpr | distToLeftExpr | distToRightExpr | nodeFnSemiExpr | edgeFnSemiExpr | phraseFnExpr | semiAttributeFnExpr | phraseWdsExpr |
    prefNGramExpr | sufNGramExpr | sentPosExpr | wdLenExpr | wdFnNormExpr | wdPropPrefixFnExpr | govFnExpr
    
  def govFnExpr : Parser[FeatureFn]       = "govFns" ^^ {_ => govenorFns _}
  
  val cc = IncrementalMurmurHash.hash("parGram") 
  
  def govenorFns(s: Int, sarr: SourceSequence[Obs], pos: Int) = {
    val gov = sarr(pos).info.get("gov")
    val gi = gov.toInt
    if (gi > 0) {
      val parent = sarr(gi-1)
      new FeatureReturn(BuiltFeature(cc) @@ sarr(pos).code @@ parent.code)
    } else new FeatureReturn
  }
}

object ArcLabelingTask {
  
  def main(argv: Array[String]) : Unit = {
    val tagger = new ArcLabeler(argv) 
    tagger.process()
  }

}