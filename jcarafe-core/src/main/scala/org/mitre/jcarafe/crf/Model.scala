/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap
import scala.collection.immutable.IntMap
import cern.colt.map.OpenLongObjectHashMap;
import org.mitre.jcarafe.util._

case class ModelMetaData(val ver: String, val date: String, val user: String)

case class ModelAuxiliaries(lex: Option[BloomLexicon],
  wdProps: Option[WordProperties],
  wdScores: Option[WordScores],
  inducedFs: Option[InducedFeatureMap])

abstract class Model(
  val fspec: String, // string format version of feature extraction specification
  val beg: Boolean,
  val lex: Option[BloomLexicon],
  val segSize: Int,
  val labelAlphabet: Alphabet[AbstractLabel],
  val crf: CoreModel) {

  def print(f: java.io.File): Unit
  def fixAlphabet(t: Boolean) = labelAlphabet.fixed_=(t)
}

class MaxEntModel(
  labelAlphabet: Alphabet[AbstractLabel],
  crf: CoreModel,
  val fsetMap: Alphabet[Long],
  val inducedMap: Option[InducedFeatureMap] = None) extends Model("", false, None, 0, labelAlphabet, crf) {
  def print(f: java.io.File) = println("--no printing available--")
}

class StdModel(
  fspec: String, // string format version of feature extraction specification
  beg: Boolean,
  val aux: ModelAuxiliaries,
  segSize: Int,
  labelAlphabet: Alphabet[AbstractLabel],
  crf: CoreModel,
  val fsetMap: OpenLongObjectHashMap) extends Model(fspec, beg, aux.lex, segSize, labelAlphabet, crf) {
  
  def getLabelAlphabet = labelAlphabet

  def this(fs: String, b: Boolean, l: Option[BloomLexicon], wp: Option[WordProperties], ws: Option[WordScores], ifs: Option[InducedFeatureMap],
    ss: Int, lalpha: Alphabet[AbstractLabel], crf: CoreModel, fsmap: OpenLongObjectHashMap) =
    this(fs, b, ModelAuxiliaries(l, wp, ws, ifs), ss, lalpha, crf, fsmap)

  def wdProps = aux.wdProps
  def wdScores = aux.wdScores
  def inducedFs = aux.inducedFs

  class GetFaMap(val a: LongAlphabet) extends cern.colt.function.LongObjectProcedure {
    def apply(fn: Long, v: java.lang.Object) = {
      v match {
        case ft: FeatureType =>
          ft.fdetail foreach { fea => a update (fea.prv, fea.cur, fn) }
        case _ =>
      }
      true
    }
  }

  def deriveFaMap: LongAlphabet = {
    val a = new LongAlphabet
    fsetMap.forEachPair(new GetFaMap(a))
    a
  }

  def adjustParameter(fname: Long, lab: AbstractLabel, fac: Double) = {
    val labId = labelAlphabet(lab)
    val ft =
      fsetMap.get(fname) match {
        case ft: FeatureType => ft.fdetail.foreach { f => if (f.cur == labId) crf.params(f.fid) += fac }
        case _ =>
      }
  }

  def adjustParameter(fname: String, lab: AbstractLabel, fac: Double): Unit = adjustParameter(fname.hashCode, lab, fac)

  def print(mfile: java.io.File) = {
    val f_out = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(mfile)), "UTF-8")
    val invLa = labelAlphabet.getInvMap
    f_out.write("Using the following feature specification:\n")
    f_out.write(fspec)
    f_out.write("\n")
    f_out.write("State alphabet\n-------------\n")
    labelAlphabet.mp foreach { case (x, _) => f_out.write(x.toString); f_out.write("\n") }
    f_out.write("-------------\n")
    f_out.flush
    f_out.close
  }
}

class RandomStdModel(
  fspec: String, // string format version of feature extraction specification
  beg: Boolean,
  aux: ModelAuxiliaries,
  segSize: Int,
  labelAlphabet: Alphabet[AbstractLabel],
  crf: CoreModel,
  val faMap: RandomLongAlphabet) extends StdModel(fspec, beg, aux, segSize, labelAlphabet, crf, new OpenLongObjectHashMap) {

  def this(fs: String, b: Boolean, l: Option[BloomLexicon], wp: Option[WordProperties], ws: Option[WordScores], ifs: Option[InducedFeatureMap],
    ss: Int, lalpha: Alphabet[AbstractLabel], crf: CoreModel, faMap: RandomLongAlphabet) =
    this(fs, b, ModelAuxiliaries(l, wp, ws, ifs), ss, lalpha, crf, faMap)
  override def deriveFaMap = faMap
  
  override def adjustParameter(fname: Long, lab: AbstractLabel, fac: Double) = {
    val labelId = labelAlphabet.get(lab).getOrElse(0)
    val fid = faMap.update(labelId,fname)
    crf.params(fid) += fac
  }
}

class NonFactoredModel(fspec: String, lex: Option[BloomLexicon], val wp: Option[WordProperties], ss: Int, crf: CoreModel, val faMap: LongAlphabet, val numStates: Int)
  extends Model(fspec, true, lex, ss, (new Alphabet[AbstractLabel]), crf) {

  class PrintFaMap extends cern.colt.function.LongIntProcedure {
    def apply(f: Long, i: Int) = {
      println("f: " + f + " i: " + i)
      true
    }
  }

  def print(mfile: java.io.File) = {
    val f_out = new java.io.BufferedWriter(new java.io.FileWriter(mfile))
    val invLa = labelAlphabet.getInvMap
    f_out.write("Using the following feature specification:\n")
    f_out.write(fspec)
    f_out.write("\n")
    f_out.write("State alphabet\n")
    labelAlphabet.mp foreach { case (x, _) => f_out.write(x.toString); f_out.write("\n") }
    f_out.write("params:")
    faMap.getUnderlyingMap.forEachPair(new PrintFaMap)
    //faMap foreach {case (fn,ft) => f_out.write("'" + fn + "' [" + ft + "] => " + crf.params(ft) + "\n")}
    f_out.flush
    f_out.close
  }
}

class RandomNonFactoredModel(fspec: String, lex: Option[BloomLexicon], wp: Option[WordProperties], ss: Int, crf: CoreModel, faMap: RandomLongAlphabet, numStates: Int)
  extends NonFactoredModel(fspec, lex, wp, ss, crf, faMap, numStates)


abstract class CoreModelSerializer {
  import scala.collection.mutable.HashMap
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}
  import com.twitter.chill.{EmptyScalaKryoInstantiator, AllScalaRegistrar}

  val instantiator = new EmptyScalaKryoInstantiator
  val kryo = instantiator.newKryo
  new AllScalaRegistrar()(kryo)
  kryo.register(classOf[CrfInstance])
  kryo.register(classOf[Alphabet[Long]])
  kryo.register(classOf[MaxEntModel])  


  protected def checkModel(m: Model) = {
    import FastLoops._
    val params = m.crf.params
    var ok = true
    forIndex(params.length) { i =>
      val k = params(i)
      if (k != params(i))
        ok = false
    }
    if (!ok) println("\n\n!!!!WARNING!!!! training resulted in model parameters having value NaN")
  }
}

object InducedFeatureMapProtocol {
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}
  import com.twitter.chill.{EmptyScalaKryoInstantiator, AllScalaRegistrar}
  import collection.mutable.HashMap

  val instantiator = new EmptyScalaKryoInstantiator
  val kryo = instantiator.newKryo
  
  def readFMap(kInput: KInput) : HashMap[Long,Array[Double]] = {
    val m = kryo.readObject(kInput, classOf[HashMap[Long,Array[Double]]])
    kInput.close()
    m
  }
  
  def readFMap(f: java.io.File): HashMap[Long,Array[Double]] = {
    val is = new java.io.BufferedInputStream(new java.io.FileInputStream(f))
    val m = readFMap(is)
    is.close()
    m
  }

  def readFMap(is: java.io.InputStream): HashMap[Long,Array[Double]] = {
    val kInput = new KInput(is)
    readFMap(kInput)
  }
  
  def readFMap(ba: Array[Byte]): HashMap[Long,Array[Double]] = readFMap(new KInput(ba))
  def readFMap(s: String): HashMap[Long,Array[Double]] = readFMap(new java.io.File(s))
  
  def writeFMap(m: HashMap[Long,Array[Double]], f: java.io.File) = {
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val output = new KOutput(os)
    kryo.writeObject(output, m)
    os.close()
    output.close
  }
  
}

object MaxEntSerializer extends CoreModelSerializer {
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}
  
  def writeModel(m: MaxEntModel, f: java.io.File) = {
    checkModel(m)
    val am = Model.compactMEModel(m)
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val output = new KOutput(os)
    kryo.writeObject(output, am)
    os.close()
    output.close
  }
  
  def serializeAsBytes(m: MaxEntModel) : Array[Byte] = {
    val out = new KOutput
    kryo.writeObject(out, m)
    out.toBytes()    
  }


  def readModel(kInput: KInput) : MaxEntModel = {
    val m = kryo.readObject(kInput, classOf[MaxEntModel])
    kInput.close()
    m
  }
  def readModel(f: java.io.File): MaxEntModel = {
    val is = new java.io.BufferedInputStream(new java.io.FileInputStream(f))
    val m = readModel(is)
    is.close()
    m
  }
  
  def readModel(is: java.io.InputStream): MaxEntModel = {
    val kInput = new KInput(is)
    readModel(kInput)
  }
  
  def readModel(ba: Array[Byte]): MaxEntModel = readModel(new KInput(ba))
  def readModel(s: String): MaxEntModel = readModel(new java.io.File(s))

}

object NonFactoredSerializer extends CoreModelSerializer {
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}

  class GetLongAlpha(val lb: collection.mutable.ListBuffer[(Long, Int)]) extends cern.colt.function.LongIntProcedure {
    def apply(fn: Long, i: Int) = {
      lb append ((fn, i))
      true
    }
  }
  
  def readModel(kInput: KInput) : NonFactoredModel = {
    val m = kryo.readObject(kInput, classOf[NonFactoredModel])
    kInput.close()
    m
  }
  def readModel(f: java.io.File): NonFactoredModel = {
    val is = new java.io.BufferedInputStream(new java.io.FileInputStream(f))
    val m = readModel(is)
    is.close()
    m
  }
  
  def readModel(is: java.io.InputStream): NonFactoredModel = {
    val kInput = new KInput(is)
    readModel(kInput)
  }
  
  def readModel(ba: Array[Byte]): NonFactoredModel = readModel(new KInput(ba))
  def readModel(s: String): NonFactoredModel = readModel(new java.io.File(s))

  def writeModel(m: NonFactoredModel, f: java.io.File) = {
    checkModel(m)
    //val am = Model.compactMEModel(m)
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val output = new KOutput(os)
    kryo.writeObject(output, m)
    os.close()
    output.close
  }
  
  def serializeAsBytes(m: NonFactoredModel) : Array[Byte] = {
    val out = new KOutput
    kryo.writeObject(out, m)
    out.toBytes()    
  }

  //def serializeAsBytes(m: NonFactoredModel) = Operations.toByteArray[NonFactoredModel](m)

  def readRModel(kInput: KInput) : RandomNonFactoredModel = {
    val m = kryo.readObject(kInput, classOf[RandomNonFactoredModel])
    kInput.close()
    m
  }
  def readRModel(f: java.io.File): RandomNonFactoredModel = {
    val is = new java.io.BufferedInputStream(new java.io.FileInputStream(f))
    val m = readRModel(is)
    is.close()
    m
  }
  
  def readRModel(is: java.io.InputStream): RandomNonFactoredModel = {
    val kInput = new KInput(is)
    readRModel(kInput)
  }
  
  def readRModel(ba: Array[Byte]): RandomNonFactoredModel = readRModel(new KInput(ba))
  def readRModel(s: String): RandomNonFactoredModel = readRModel(new java.io.File(s))

  def writeModel(m: RandomNonFactoredModel, f: java.io.File) = {
    checkModel(m)
    //val am = Model.compactMEModel(m)
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val output = new KOutput(os)
    kryo.writeObject(output, m)
    os.close()
    output.close
  }
    
}

object StandardSerializer extends CoreModelSerializer {
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}
  
  class FTypeBuilder(val lbuf: collection.mutable.ListBuffer[(Long, FeatureType)]) extends cern.colt.function.LongObjectProcedure {
    def apply(k: Long, v: java.lang.Object) = {
      v match {
        case v: FeatureType => lbuf append ((k, v))
        case _ =>
      }
      true
    }
  }


  def writeModel(m: StdModel, f: java.io.File): Unit = {
    def writeToStream[M](os: java.io.OutputStream, hs: String, m: M) = {
      val headerBytes = hs.getBytes()
      os.write(headerBytes)
      val kout = new KOutput(os)
      kryo.writeObject(kout,m)
      kout.close()
      os.close()
    }
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    m match {
      case model: RandomStdModel => writeToStream(os, "RAND\n", model)
      case model: StdModel => writeToStream(os, "STAN\n", model) 
    }
  }

  def getFirstNBytes(bis: java.io.BufferedInputStream, n: Int) = {
    var e = true
    var c = 0
    val bBuf = new collection.mutable.ArrayBuffer[Byte]()
    while (c < n) {
      c += 1
      val b = bis.read()
      bBuf append b.toByte
    }
    bis.read() // this should be a newline
    bBuf.toArray
  }
  
  def readStdFromByteArray(b: Array[Byte]) : StdModel = {
    val kInput = new KInput(b)
    val r = kryo.readObject(kInput, classOf[StdModel])
    kInput.close()
    r
  }

  def readRandomFromByteArray(b: Array[Byte]) : RandomStdModel = {
    val kInput = new KInput(b)
    val r = kryo.readObject(kInput, classOf[RandomStdModel])
    kInput.close()
    r
  }

  def readModel(in: java.io.InputStream): StdModel = {
    val is = new java.io.BufferedInputStream(in)
    val firstFourBytes = getFirstNBytes(is, 4)
    val firstLine = new String(firstFourBytes)
    val arBuf = new collection.mutable.ArrayBuffer[Byte]()
    var nRead: Int = 0
    if (!(firstLine equals "RAND") && !(firstLine equals "STAN"))
      arBuf appendAll firstFourBytes
    while (nRead != (-1)) {
      nRead = is.read()
      if (nRead >= 0) arBuf append nRead.toByte
    }
    val byteArray = arBuf.toArray
    if (firstLine equals "RAND") readRandomFromByteArray(byteArray)
    else readStdFromByteArray(byteArray)
  }

  //def serializeAsBytes(m: StdModel) = Operations.toByteArray[StdModel](m)
  def readModel(f: java.io.File): StdModel = readModel(new java.io.FileInputStream(f))
  //def readModel(r: java.io.InputStream): StdModel = Operations.read[StdModel](r)

  def readModel(s: String): StdModel = readModel(new java.io.File(s))
  def readModel(o: Option[String]): StdModel = o match {
    case Some(s) => readModel(s)
    case None => throw new RuntimeException("Model Not specified")
  }
}

object Model {

  import org.mitre.jcarafe.util.FastLoops._

  val modelVersion = "Model Version 1.1"

  class FTypeForEachAdd(val idMap: scala.collection.mutable.HashMap[Int, (Int, Double)]) extends cern.colt.function.LongObjectProcedure {
    val nfsetMap = new OpenLongObjectHashMap
    def apply(fname: Long, v: java.lang.Object) = {
      v match {
        case ftype: FeatureType =>
          val nftype = new FeatureType(fname, ftype.edgep, ftype.segsize)
          ftype.fdetail foreach { f =>
            if (idMap.contains(f.fid)) {
              val (nid, v) = idMap(f.fid)
              nftype add new Feature(f.prv, f.cur, nid)
            }
          }
          if (nftype.fdetail.size > 0) nfsetMap.put(fname, nftype) // add feature type if at least one is non-zero  
        case _ =>
      }
      true
    }
  }

  /*
   * Remove entries from model that have parameter values of 0.0
  */
  def compactModel(model: StdModel): StdModel = {
    val idMap = new scala.collection.mutable.HashMap[Int, (Int, Double)]
    var i = 0
    var j = 0
    while (i < model.crf.params.length) {
      if ((model.crf.params(i) < 10e-40) && (model.crf.params(i) > -10e-40)) j += 1 // a zero parameter
      else {
        idMap += ((i, ((i - j), model.crf.params(i)))) // map each feature to a new id
      }
      i += 1
    }
    val narr = Array.fill(idMap.size)(0.0)
    idMap foreach { case (k, v) => narr(v._1) = v._2 }
    val nfsetMap = new OpenLongObjectHashMap()
    val ftypeForEach = new FTypeForEachAdd(idMap)
    model.fsetMap.forEachPair(ftypeForEach)
    new StdModel(model.fspec, model.beg, model.lex, model.wdProps, model.wdScores, model.inducedFs, model.segSize, model.labelAlphabet, new CoreModel(narr, narr.length, model.crf.nls), ftypeForEach.nfsetMap)
  }

  def compactMEModel(model: MaxEntModel): MaxEntModel = {
    val idMap = new scala.collection.mutable.HashMap[(Int, Int), (Int, Double)]
    var i = 0
    var j = 0
    val nfs = model.crf.params.length / model.crf.nls
    forIndex(nfs) { i =>
      // need to remove all (or none of the) parameters associated with a feature (i.e. for all classes)
      var remove = false
      forIndex(model.crf.nls) { k =>
        val offset = nfs * k + i
        if ((model.crf.params(offset) < 10e-40) && (model.crf.params(offset) > -10e-40))
          remove = true
      }
      if (remove) {
        j += 1 // a zero parameter
      } else {
        forIndex(model.crf.nls) { k =>
          idMap += (((i, k), ((i - j), model.crf.params(nfs * k + i))))
        }
      }
    }
    val newNfs = nfs - j
    val narr = Array.fill(model.crf.params.length - (j * model.crf.nls))(0.0)
    idMap foreach {
      case ((i, k), (nid, v)) =>
        val anid = nid + k * newNfs
        narr(anid) = v
    }
    val nfsetMap = new Alphabet[Long]()
    model.fsetMap.mp foreach {
      case (fid, f) =>
        idMap.get((f, 0)) match {
          case Some((nf, v)) => nfsetMap.update(fid, nf)
          case None =>
        }
    }
    new MaxEntModel(model.labelAlphabet, new CoreModel(narr, narr.length, model.crf.nls), nfsetMap, model.inducedMap)
  }

  def combineModels(mList: List[StdModel]): Model = {
    throw new RuntimeException("combine Models needs to yet be optimized")
    /*
    val nparams = new HashMap[Int,Double]
    var newFsetMap = IntMap[FeatureType]()
    val nLa = new Alphabet[AbstractLabel]
    val nFa = new Alphabet[PreFeature]
    val scaleLa = new HashMap[AbstractLabel,Int]  // keep track of how often particular labels/states occur across the set of models
    def updateParams(nid:Int, v: Double) = 
      if (nparams contains nid)
        nparams(nid) += v
      else nparams += ((nid, v))
    mList foreach {m => m.labelAlphabet foreach {case (l,i) => 
        if (scaleLa contains l) scaleLa(l) += 1 else scaleLa += ((l, 1)) }}

    mList foreach {m =>
      val invLocalMap = m.labelAlphabet.getInvMap
      m.fsetMap foreach {case (fname,ftype) => 
        val nfeature = 
          if (newFsetMap contains fname) newFsetMap(fname)
          else {
            val nf = new FeatureType(fname,ftype.edgep,ftype.segsize)
            newFsetMap = newFsetMap.updated(fname, nf)
            nf
          }
        ftype.fdetail foreach {fea => // case Feature(prv,cur,fid,v) =>
          val curLab = invLocalMap(fea.cur)
          val n_cur = nLa update curLab
          val n_prv = if (fea.prv < 0) -1 else nLa update invLocalMap(fea.prv)
          val n_fid = nFa update PreFeature(n_prv, n_cur, fname)
          updateParams(n_fid,m.crf.params(fea.fid) / pow(scaleLa(curLab),1.25))
          nfeature add new Feature(n_prv,n_cur,n_fid,1.0)
        }
      }
    }
    val newParams = Array.tabulate(nparams.size){(i:Int) => nparams(i)}
    val newCrf = new CoreModel(newParams,newParams.length,nLa.size)
    mList match {
      case first :: _ => 
        new StdModel(first.fspec, first.beg, first.lex, first.preProc, first.segSize, nLa, newCrf, newFsetMap)
      case Nil => throw new RuntimeException("Model combination expects at least one input model") }
      * */
  }

}
