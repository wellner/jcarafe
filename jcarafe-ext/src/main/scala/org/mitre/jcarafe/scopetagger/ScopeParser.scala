package org.mitre.jcarafe.scopetagger

import org.mitre.jcarafe.tagger.StdTaggerTask
import org.mitre.jcarafe.crf.FactoredTrainer
import org.mitre.jcarafe.crf.FactoredDecoder
import org.mitre.jcarafe.crf.StdDecoder
import org.mitre.jcarafe.crf.TrainingSeqGen
import org.mitre.jcarafe.crf.FactoredDecodingSeqGen
import org.mitre.jcarafe.crf.TextSeqGen
import org.mitre.jcarafe.crf.SeqGenScorer
import org.mitre.jcarafe.crf.StdTrainer
import org.mitre.jcarafe.crf.StdDecoder
import org.mitre.jcarafe.crf.TrainingFactoredFeatureRep
import org.mitre.jcarafe.crf.DecodingFactoredFeatureRep
import org.mitre.jcarafe.crf.FeatureManager
import org.mitre.jcarafe.crf.BloomLexicon
import org.mitre.jcarafe.crf.StandardSerializer
import org.mitre.jcarafe.util.Annotation
import org.mitre.jcarafe.tokenizer._
import org.mitre.jcarafe.util._
import scala.collection.immutable.IndexedSeq

trait ScopeDecodeHandler extends OptionHandler {
  "--cue-model" desc "Model for cue phrase detection"
  "--decode-raw" flag "Run decoder on raw files and use i2b2 standoff format"
  "--decode-raw-inline" flag "Run decoder on raw files and write results inline"
}

class Counter(var x: Int) {
  def this() = this(0)
  def ++ = { x += 1}
  def value = x
}


case class IndexedAnnot(val a: Annotation,val i: Int) extends Ordered[IndexedAnnot] {
  def compare(that: IndexedAnnot) = a.compare(that.a)
  def equals(that: IndexedAnnot) = a.equals(that.a)
  override def hashCode = (a.hashCode * i)
}

class ScopeOptions(a: Array[String]) extends Options(a,new OptionHandler(a) with ScopeDecodeHandler) {
  val cueModel = optHandler.get("--cue-model")
  val decodeRaw = optHandler.check("--decode-raw")
  val decodeRawInline = optHandler.check("--decode-raw-inline")
}

class ScopeParser(opts: Options) extends StdTaggerTask(opts) {
  
  import StandardSerializer._

  override def getTrainer() = {
    new StdTrainer(opts) {
      val fspecStr = FeatureManager.getFeatureSpecString(opts.featureSpec.get)
      val mgr = new ScopeFeatureManager(fspecStr)
      mgr.lex match {case None => opts.lexDir match {case Some(d) => mgr.lex_=(Some(new BloomLexicon(d))) case None =>} case Some(_) => }
      val fr = new TrainingFactoredFeatureRep[String](mgr, opts)
      val sGen = new TrainingSeqGen[String] (fr, opts) with ScopeSeqGen
    }	
  }

  override def getDecoder(modelFile: String, eval: Boolean, preModel: Boolean = false) : StdDecoder = {
    new StdDecoder(opts) {
      val mgr = new ScopeFeatureManager(model.fspec)
      val fr = new DecodingFactoredFeatureRep[String](mgr, opts, model)
      val eval = opts.evaluate.isDefined
      val sGen : FactoredDecodingSeqGen[String] = 
	if (eval) new FactoredDecodingSeqGen[String] (fr, model,opts) with ScopeSeqGen with SeqGenScorer[String]
	else new FactoredDecodingSeqGen[String] (fr, model,opts) with ScopeSeqGen
      setDecoder(true)
    }
  }
  
}

object FullDecoder {
  def apply(scopeModel: String, cueModel: String) = {
    val decOpts = new ScopeOptions(Array("--cue-model",cueModel,"--model",scopeModel))
    new FullDecoder(scopeModel,decOpts)
  }
}

class FullDecoder(scopeModel: String, opts: ScopeOptions) {

  import StandardSerializer._
	
  val scopeDecoder = new ScopeDecoder(scopeModel,opts)
  val cueDecoder = new FactoredDecoder[String](opts) {
    val model = readModel(opts.cueModel.get)
    val sGen = new FactoredDecodingSeqGen[String] (model,opts) with ScopeJsonSeqGen with SeqGenScorer[String]
  }
	
  private def segmentDeserialization(d: List[Element]) : List[IndexedSeq[Element]] = {
    def gather(cur: List[Element], cs: List[Element], vecs: List[IndexedSeq[Element]]) : List[IndexedSeq[Element]] = {
      cur match {
	case (ft @ HardEndTok(t)) :: r => gather(r,Nil,((ft :: cs).reverse.toIndexedSeq) :: vecs)
	case (ft @ EndWs(t)) :: r =>  gather(r,Nil, (ft :: cs).reverse.toIndexedSeq :: vecs)
	case (ft @ SoftEndTok(t)) :: (fw @ Ws(w)) :: (fw1 @ EndWs(w1)) :: r => gather(r,Nil,(fw1 :: fw :: ft :: cs).reverse.toIndexedSeq :: vecs) 
	case (ft @ SoftEndTok(t)) :: (fw @ EndWs(w)) :: r => gather(r,Nil,(fw :: ft :: cs).reverse.toIndexedSeq :: vecs)
	case (ft @ SoftEndTok(t)) :: (fw @ Ws(w)) :: r => gather(r,Nil,(fw :: ft :: cs).reverse.toIndexedSeq :: vecs)
	case t :: r => gather(r,(t :: cs),vecs)
	case Nil => vecs.reverse
      }
    }
    gather(d,Nil,Nil)
  }
  
  private def popStck(s: String, sb: StringBuilder, ist: scala.collection.mutable.Stack[Int], cp: Int) : Unit = 
    if (!ist.isEmpty && (ist.top <= cp)) {sb append s; ist.pop; popStck(s,sb,ist,cp) }
  
  def annotsToInLine(signal: String, annots: List[IndexedAnnot]) : String = {
    val sorted = annots.sortWith(_<_)
    val sb = new StringBuilder
    val cueEndStack = new scala.collection.mutable.Stack[Int]
    val scopeEndStack = new scala.collection.mutable.Stack[Int]
    def write(p: Int,curAnnots: List[IndexedAnnot]) : Unit = curAnnots match {
      case IndexedAnnot(a1,v1) :: r if (a1.st == p) =>
	val at = a1.typ match { case Label("cue",as) => ("cue",(Some (as("type")))) case SLabel(l) => (l,None) case _ => throw new RuntimeException("Unexpected annot type") }
        at match {
	  case ("xcope",None) => sb append ("<xcope id=\""+v1+"\">"); scopeEndStack push a1.en
	  case ("cue",Some(x)) => sb append ("<cue type=\""+x+"\" ref=\"" + v1 + "\">"); cueEndStack push a1.en
	  case _ => throw new RuntimeException("Unexpected annot type") 
	}
        write(p,r)
      case a if (p < signal.length) => 
	popStck("</cue>",sb,cueEndStack,p)
        popStck("</xcope>",sb,scopeEndStack,p)
        sb append signal(p);
        write(p+1,a)
      case _ =>
    }
    write(0,sorted)
    sb.toString
  }

  private def buildTokenIndex(ts: Seq[Element]) : Map[Int,Int] = {
    var pos = 0
    var id = 0
    ts.foldLeft(Map():Map[Int,Int]){(ac,e) =>
      e match { 
	case Tok(t) => 
	  val nt = ac + (pos -> id) + ((pos+t.size) -> id)
	  id += 1
	  pos += t.size
	  nt
	case a => pos += a.getString.size; ac}}
  }

  private def annotsToi2b2Format(sid: Int, annots: Seq[IndexedAnnot], os: java.io.Writer) = {
    annots foreach { 
      case IndexedAnnot(an,cid) =>
	val at = an.typ match { case Label("cue",as) => ("cue",(Some (as("type")))) case SLabel(l) => (l,None) case _ => throw new RuntimeException("Unexpected annot type") }
        at match {
	  case ("xcope",None) => os.write("c=\"\" " + sid + ":" + an.st + " " + sid + ":" + an.en + "||t=\"xcope\"||id=\""+cid+ "\"")
	  case ("cue",Some(x)) => os.write("c=\"\" " + sid + ":" + an.st + " " + sid + ":" + an.en + "||t=\"cue\"||sub_t=\""+x+"\" ref=\""+cid+ "\"")
	  case _ => throw new RuntimeException("Unexpected annotation type") }
        os.write('\n')
    }
  }


  def decodeSent(ci: Counter, i2b2: Boolean, ses: Seq[Element], line: Int) : Seq[IndexedAnnot] = 
    decodeSent(ci,i2b2,ses,ses.foldLeft(""){_ + _.getString}, line)

  def decodeSent(ci: Counter, i2b2: Boolean, ses: Seq[Element], str: String, line: Int) : Seq[IndexedAnnot] = {
    val decodedCueAnnots = cueDecoder.decodeStringToAnnotations(Json.writeJsonToString(JsObject(Map("signal" -> JsString(str))))).toSeq
    val joinedAnnots = 
      decodedCueAnnots flatMap 
      { an =>
	val semType = try an.typ.assoc("type") catch { case e => "neg" }
        val tp = Json.writeJsonToString(
	    JsObject(Map("signal" -> JsString(str), "asets" -> 
            		 JsArray(List(JsObject(Map("type" -> JsString("lex"), 
            			 	 	   "attrs" -> JsArray(List(JsString("type"))),
            			 	 	   "annots" -> JsArray(List(JsArray(JsInt(an.st) :: JsInt(an.en) :: JsString(semType) :: Nil))))))))))
        val scAs = scopeDecoder.decodeStringToAnnotations(tp)
        ci ++ 
        val scA = if (scAs.length > 0) scAs(0) else new Annotation(an.st,str.length-1,false,SLabel("xcope"))
        List(IndexedAnnot(scA,ci.value),IndexedAnnot(an,ci.value))
     }
    if (i2b2) {
      val mapTbl = buildTokenIndex(ses)
      joinedAnnots map {
	case IndexedAnnot(an,ci) => 
	  val indSt = mapTbl.get(an.st).getOrElse(-1)
	  val indEn_p = mapTbl.get(an.en).getOrElse(indSt) + 1 
	  val indEn = if (indEn_p >= (ses.length - 2)) ses.length else indEn_p
	  IndexedAnnot(new Annotation(indSt,indEn,an.beg,an.typ,Some(line.toString)),ci)}
	 //annotsToi2b2Format(sInd+1,i2b2Annots,os)
       } else 
	 joinedAnnots.toSeq
         //val ns = annotsToInLine(s,joinedAnnots.toList)
  }

  def decodeDocument(toks: List[List[String]]) : Seq[IndexedAnnot] = {
    val sentElements = toks map {sent => (sent.foldLeft(Nil: List[Element]) {(ac,t) => Ws(" ") :: Tok(t) :: ac}).reverse}
    val c = new Counter
    sentElements.zipWithIndex flatMap {case (ses,sInd) => decodeSent(c,true,ses,sInd+1)}
  }

  def decodeDocument(toks: Array[Array[String]]) : java.util.List[IndexedAnnot] = {
    import scala.collection.JavaConversions._
    val ltoks = (toks map {_.toList}).toList
    decodeDocument(ltoks) 
  }

  def decodeFile(i2b2: Boolean, f: java.io.File, ofile: java.io.File) : Unit = {
    val deser = WhiteSpaceTokenizer.parseFile(f.toString)
    val ostr = new java.io.FileOutputStream(ofile)    
    val os = new java.io.OutputStreamWriter(ostr)
    val sentElements = segmentDeserialization(deser)
    val c = new Counter
    sentElements.zipWithIndex foreach {case (ses,sInd) =>
      val annots = decodeSent(c,i2b2,ses,sInd+1)
      if (i2b2) annotsToi2b2Format(sInd+1,annots,os)
      else os.write(annotsToInLine(ses.foldLeft(""){_ + _.getString},annots.toList))
      }
    os.close
  }
  
  def decode(i2b2: Boolean = true) = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match { 
          case Some(r) =>
            new scala.util.matching.Regex(r) 
          case None => new scala.util.matching.Regex(".*") }
        val dir = new java.io.File(dirStr)
        val odir = opts.outputDir
        dir.listFiles filter 
        {f:java.io.File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false}} foreach 
        {f:java.io.File =>
	  val osuffix = opts.outSuffix match {case Some(o) => o case None => ""} 
	  val ofile = odir match {case Some(d) => Some(d + "/" + f.getName + osuffix) case None => None }
	  ofile match {case Some(ofile) => decodeFile(i2b2,f,new java.io.File(ofile)) case None => }}
      case None => 
	opts.inputFile match {
	  case Some(f) => decodeFile(i2b2,new java.io.File(f),new java.io.File(opts.outputFile.getOrElse(f + ".OUT")))
	  case None => throw new RuntimeException("Expected output file")}
    }
  }
}


class ScopeDecoder(scopeModel: String, opts: Options) extends FactoredDecoder[String](opts) {
  import StandardSerializer._

  val model = readModel(scopeModel)
  val mgr = new ScopeFeatureManager(model.fspec)
  val fr = new DecodingFactoredFeatureRep[String](mgr, opts, model)
  val sGen : FactoredDecodingSeqGen[String] = new FactoredDecodingSeqGen[String] (fr, model,opts) with ScopeJsonSeqGen with SeqGenScorer[String]
  setDecoder(true)
}

class ScopeDecoderParser(argv: Array[String]) {
  val opts = new ScopeOptions(argv)
  val decoder = new FullDecoder(opts.model.get, opts)
  def decode(i2b2: Boolean = true) = decoder.decode(i2b2)
}

object ScopeParserMain {
  def printUsage = 
    println(" Usage: java -cp jcarafe-0.9.X.RCX.jar org.mitre.jcarafe.scopetagger.ScopeParserMain [train|decode|decode-raw|decode-raw-inline] <options>")
  
  def main(argv: Array[String]) : Unit = {
    val opts = new ScopeOptions(argv)
    if (opts.decodeRaw) {
      val sdp = new ScopeDecoderParser(argv)
      sdp.decode()
    } else if (opts.decodeRawInline) {
      val sdp = new ScopeDecoderParser(argv)
      sdp.decode(false)
    } else {
      val tagger = new ScopeParser(opts) 
      tagger.process()
    }
  }
}

