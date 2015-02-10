package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{StdTrainer,StdDecoder,TrainingSeqGen,FactoredDecodingSeqGen,StdModel,
  StandardSerializer,TrainingFactoredFeatureRep,DecodingFactoredFeatureRep,SourceSequence,FeatureReturn,IncrementalMurmurHash,
  BuiltFeature,FeatureManager, FeatureManagerBuilder, DynamicFeatureManagerBuilder, FeatureFn}
import org.mitre.jcarafe.util.Options
import org.mitre.jcarafe.tagger.TaggerTask

class ArcLabeler(argv: Array[String]) extends TaggerTask[String](argv) {
  
  lazy val trainer = new StdTrainer(opts) {
    val builder = new ArcLabelingFeatureManagerBuilder[String](FeatureManagerBuilder.getFeatureSpecString(opts.featureSpec.get))
    val mgr = builder.getFeatureManager
    val fr = new TrainingFactoredFeatureRep[String](mgr,opts)
    val sGen = new TrainingSeqGen[String](fr,opts) with CoNLLSeqGenLabel
  } 
  
  lazy val decoder = new StdDecoder(opts) {
    val builder = new ArcLabelingFeatureManagerBuilder[String](opts.featureSpec.get)
    val mgr = builder.getFeatureManager
    val fr = new DecodingFactoredFeatureRep[String](mgr,opts,model,false)
    val sGen = new FactoredDecodingSeqGen[String](fr,model,opts) with CoNLLSeqGenLabel
  }

}

class ArcLabelingFeatureManagerBuilder[Obs](iString: String) extends DynamicFeatureManagerBuilder[Obs](iString) {
  
  override def simpleFnExpr : Parser[FeatureFn[Obs]] = 
    predicateExpr | prefFnExpr | sufFnExpr | wdFnExpr | caseLessFnExpr | lexFnExpr | wdPropFnExpr | downWdPropFnExpr | wdScoreFnExpr |
    downLexFnExpr | nodeFnExpr | edgeFnExpr | regexpFnExpr | allTagFnExpr | antiPrefFnExpr | antiSufFnExpr | attributeFnExpr |
    weightedAttrExpr | distToLeftExpr | distToRightExpr | nodeFnSemiExpr | edgeFnSemiExpr | phraseFnExpr | semiAttributeFnExpr | phraseWdsExpr |
    prefNGramExpr | sufNGramExpr | sentPosExpr | wdLenExpr | wdFnNormExpr | wdPropPrefixFnExpr | govFnExpr
    
  def govFnExpr : Parser[FeatureFn[Obs]]       = "govFns" ^^ {_ => govenorFns _}
  
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
