/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._

trait NonFactoredMaxEntCore {
  
  def classScoresNormalizedNF(nls: Int, nfs:Int, lambdas:collection.mutable.IndexedSeq[Double], sparseFeatures: Seq[Feature]) = {
    val unScores = Array.fill(nls)(0.0)
    sparseFeatures foreach {f => unScores(f.cur) += f.value * lambdas(f.fid)}
    var sum = 0.0
    val mx = unScores.foldLeft(-Double.MaxValue){(ac,v) => if (v > ac) v else ac}
    val preNorm =  unScores map {v =>
      val ev = scala.math.exp(v - mx);
      sum += ev; ev }
    preNorm map {v => v/sum}
  }
}

class RankInstance(probDist: Array[Double]) extends NonFactoredCrfInstance(-1,-1,1) {
  val len = probDist.length
  private def normalize(probDist: Array[Double]) = {
    var sum = 0.0
    var c = 0; while (c < len) {
      sum += probDist(c)
      c += 1
    }
    c = 0; while (c < len) {
      probDist(c) /= sum
      c += 1
    }
  }
  normalize(probDist)
  for (i <- 0 until len) {
    condProbTbl += (i -> probDist(i)) // initialize probability dist
  }
}

abstract class NonFactoredMaxEnt(nls: Int, nfs: Int, gPrior: Double) extends MaxEnt(nls, nfs, gPrior) with NonFactoredMaxEntCore {
  
  override def gradOfElement(el: AbstractInstance) = {
    
    val instFeatures = el.getCompVec
    val trueLabel = el.label
    val scores = classScoresNormalizedNF(el.getRange, nfs, lambdas, instFeatures(0)).toArray
    var k = 0
    while (k < instFeatures(0).length) {
      val inst = instFeatures(0)(k)
      gradient(inst.fid) += (scores(inst.cur) - el.conditionalProb(inst.cur)) * inst.value
      k += 1
    }
    k = 0
    var cost = 0.0
    while (k < el.getRange) {
      cost += scala.math.log(scores(k)) * el.conditionalProb(k)
      k += 1
    }
    cost
  }
}

trait NonFactoredSeqGen {
  val faMap: LongAlphabet

  import org.mitre.jcarafe.crf.IncrementalMurmurHash._
  
  def addMEFeature(inst:RankInstance, l: Int, fname:String, vl: Double) : Unit = {
    val fid = faMap update hash(fname)
    if (fid >= 0) {
      val f = new NBinFeature(vl,-1,l,fid)
      inst add f }
  }
    
  def addInFeatures (inst:RankInstance, dseq: SourceSequence[Array[RankReadInst]], pos:Int) : Unit = {
    var i = 0;
    while (i < dseq(pos).obs.length) {
      dseq(pos).obs(i).features foreach {case (l,v) => addMEFeature(inst,i,l,v)}
      i += 1
    }
  }
}

abstract class NonFactoredRankTrainingSeqGen(opts: Options) extends SeqGen[Array[RankReadInst]](opts) with NonFactoredSeqGen {

  type FRepT = NonFactoredFeatureRep[Array[RankReadInst]] 
  val faMap = new LongAlphabet
  val frep = new NonFactoredFeatureRep[Array[RankReadInst]](
    (new NonFactoredFeatureManager[Array[RankReadInst]]("") { val fnList = Nil; val fineFnList=Nil }),1)
    
  override def extractFeatures(dseq: SourceSequence[Array[RankReadInst]]) : InstanceSequence = {
    val iseq = Vector.tabulate(dseq.length){(i: Int) =>
        val inst = new RankInstance(dseq(i).obs map { _.p} )
        addInFeatures(inst,dseq,i)
        inst : AbstractInstance}
      InstSeq(iseq)
  }
  
  override def extractFeatures (sourcePairSeqs: Seqs) : collection.immutable.IndexedSeq[InstanceSequence] = {
    sourcePairSeqs map extractFeatures
  }
  
  def getNumberOfFeatures = faMap.size
    val boundaries = opts.boundaries
}

abstract class NonFactoredRankDecodingSeqGen(m: NonFactoredModel, opts: Options) 
extends DecodingSeqGen[Array[RankReadInst]](m:Model,opts) with NonFactoredSeqGen {
  val faMap =  m.faMap
  faMap.fixed_=(true) 
  type FRepT = NonFactoredFeatureRep[Array[RankReadInst]] 
  val frep = new NonFactoredFeatureRep[Array[RankReadInst]](
    (new NonFactoredFeatureManager[Array[RankReadInst]]("") { val fnList = Nil; val fineFnList=Nil }),1) 
  def getNumberOfFeatures = faMap.size

  def extractFeatures (dseq: SourceSequence[Array[RankReadInst]]) : InstanceSequence = {
    val iseq = 
      Vector.tabulate(dseq.length){(i: Int) => 
        val inst = new RankInstance(dseq(i).obs map { _.p} )
	addInFeatures(inst,dseq,i)
          inst : AbstractInstance}
	   InstSeq(iseq)
  }
  
  override def extractFeatures (sourcePairSeqs: Seqs) : collection.immutable.IndexedSeq[InstanceSequence] = 
    sourcePairSeqs map {dseq => extractFeatures(dseq) }
}

class NonFactoredMaxEntDecodingAlgorithm(crf: CoreModel) extends DecodingAlgorithm(crf) with NonFactoredMaxEntCore {
  def getCopyOf = new NonFactoredMaxEntDecodingAlgorithm(crf)
  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance]) = {
    iseq foreach {el =>
      val scores = classScoresNormalizedNF(el.getRange, crf.nfs , crf.params, el.getCompVec(0))
      val scoresZipped = scores.toList.zipWithIndex
      el.label = scoresZipped.foldLeft((0.0,0)){(ac,v) => if (v._1 > ac._1) v else ac}._2
      scoresZipped foreach {sc => el.setConditionalProb(sc._2,sc._1) }
    }
    0.0
  }
}

class RankReadInst(val p: Double, val lab: String, val features: List[(String,Double)])

trait MaxEntSeqGenGeneral extends MaxEntSeqGenCore[Array[RankReadInst]] {

  val instBreak = "----.*".r
  val nonEndLine = "[^\n\r]+".r

  def pullOffComment(l: String) : (String,Option[String]) =  
    l.split("#").toList match {
      case a :: b :: c =>
        val comment = nonEndLine.findFirstIn((b::c).foldLeft(""){_+_}) match {case Some(x) => x case None => throw new RuntimeException("Invalid feature line: " + l)}
      	(a,Some(comment)) 
      case a :: _ => (a,None) case Nil => throw new RuntimeException("Unexpected line: " + l)}

  override def seqsToFile(dt: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) : Unit = {
    val ostr = new java.io.FileOutputStream(f)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    var cnt = 0
    val d = dt.is
    seqs(0).iseq foreach {e =>
      var i = 0
      while (i < e.getRange) {
        if (i > 0) os.write("\n")
        val lcomp = pullOffComment(d.readLine())
        var line = lcomp._1
        var comment = lcomp._2
        instBreak.findFirstIn(line) match {
          case Some(li) => 
            os.write(li+"\n"); 
            cnt += 1;
            val lcomp1 = pullOffComment(d.readLine())
            line = lcomp1._1
            comment = lcomp1._2
          case None => }
        val lab = line.split(" ").toList match {case a :: _ => a case Nil => throw new RuntimeException("Unexpected input line")} 
        os.write(lab+"\t"+e.conditionalProb(i));
        comment match {case Some(c) => os.write("\t# " + c) case None => }
        cnt += 1	
        i += 1
      }
      os.write("\n")
    }	
    d.close()
    os.close()
  }

  override def toSources(lines: DeserializationT) : Seqs = {
    //val tmpBuf = new scala.collection.mutable.ListBuffer[ObsSource[Array[RankReadInst]]]
    val tmpBuf = new scala.collection.mutable.ListBuffer[SourceSequence[Array[RankReadInst]]]
    val localBuf = new scala.collection.mutable.ListBuffer[RankReadInst]
    var al = ""
    val instr = lines.is
    al = instr.readLine()
    while (al != null) {
      instBreak.findFirstIn(al) match { 
        case Some(_) => 
          if (!localBuf.isEmpty) {
            val rankReads: Array[RankReadInst] = localBuf.toArray
            tmpBuf += new SourceSequence(Seq(createSource(SLabel("label"),rankReads,false)))
            localBuf.clear
          }
        case None =>
          val (l,_) = pullOffComment(al) 
          l.split(" ").toList match {
          case lab :: prob :: fs => localBuf += new RankReadInst(prob.toDouble, lab,(fs map {el => el.split(":").toList match { 
          	case a :: b :: Nil => (a,b.toDouble) 
          	case a :: _ => (a,1.0) 
          	case Nil => throw new RuntimeException("Feature vector parse failed")}}))
          case _ =>
          }	
      }
      al = instr.readLine()
    }
    if (!localBuf.isEmpty) tmpBuf += new SourceSequence(Seq(createSource(SLabel("label"),localBuf.toArray,false))) 
    tmpBuf.toIndexedSeq : Seqs 
  }

}

class NonFactoredMaxEntClassifier(argv: Array[String]) {
  
  lazy val opts = new Options(argv)
  import NonFactoredSerializer._

  lazy val trainer = new Trainer[Array[RankReadInst]](opts) with LinearCRFTraining[Array[RankReadInst]] {
    type TrSeqGen = NonFactoredRankTrainingSeqGen
    val sGen = new NonFactoredRankTrainingSeqGen(opts) with MaxEntSeqGenGeneral
   	  
    def trainModel(me: Trainable[AbstractInstance], seqs: collection.immutable.IndexedSeq[InstanceSequence], modelIterFn: Option[(CoreModel,Int) => Unit] = None) = {
        val accessSeq = new MemoryAccessSeq(seqs)	
   	    val coreModel = me.train(accessSeq)
   	    val m = new NonFactoredModel(sGen.getModelName,sGen.getLexicon, sGen.getWordProps ,1,coreModel,sGen.faMap,sGen.getNumberOfStates)
   	    writeModel(m,new java.io.File(opts.model.get))
      }
    
    override def train() = {
      val seqs : collection.immutable.IndexedSeq[InstanceSequence] = sGen.createSeqsFromFiles  // this has to happen before generating CRF
      val me = new NonFactoredMaxEnt(2,sGen.faMap.size,opts.gaussian) with CondLogLikelihoodLearner[AbstractInstance]
      trainModel(me, seqs) 
    }
  }
  
  lazy val decoder = new Decoder[Array[RankReadInst]](true,opts) {
    type M = NonFactoredModel
    val model : NonFactoredModel = readModel(opts.model match {case Some(s) => new java.io.File(s) case None => throw new RuntimeException("Invalid model")})
    //val sGen = new NonFactoredRankDecodingSeqGen(model,opts) with MaxEntSeqGenCore[Array[RankReadInst]]
    val sGen = new NonFactoredRankDecodingSeqGen(model,opts) with MaxEntSeqGenGeneral
    override def decodeToAnnotations(s: String) : Array[Annotation] = throw new RuntimeException("Unavailable method")
    override def decode() = decodeSeqsFromFiles(new NonFactoredMaxEntDecodingAlgorithm(model.crf))
    setDecoder(false)
  }
  
  def printUsage() = println("java -cp ..jcarafe-...jar org.mitre.jcarafe.maxent.MERank [train|decode] <arguments>" )

  def process() = {  
    if (argv.length == 0) {printUsage(); sys.exit(2)} 
    if (opts.train) {
      trainer.train()
      println("Finished training")
    } else 
      decoder.decode()
  }
}
