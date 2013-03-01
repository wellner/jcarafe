/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.HashMap
import scala.collection.immutable.IntMap
import cern.colt.map.OpenLongObjectHashMap;
import sbinary._
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
    labelAlphabet foreach { case (x, _) => f_out.write(x.toString); f_out.write("\n") }
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
  override def deriveFaMap = faMap
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
    labelAlphabet foreach { case (x, _) => f_out.write(x.toString); f_out.write("\n") }
    f_out.write("params:")
    faMap.getUnderlyingMap.forEachPair(new PrintFaMap)
    //faMap foreach {case (fn,ft) => f_out.write("'" + fn + "' [" + ft + "] => " + crf.params(ft) + "\n")}
    f_out.flush
    f_out.close
  }
}

class RandomNonFactoredModel(fspec: String, lex: Option[BloomLexicon], wp: Option[WordProperties], ss: Int, crf: CoreModel, faMap: RandomLongAlphabet, numStates: Int)
extends NonFactoredModel(fspec, lex, wp, ss, crf, faMap, numStates)

object InducedFeatureMapProtocol extends DefaultProtocol {

  implicit def fMap: Format[HashMap[Long, Array[Double]]] =
    wrap[HashMap[Long, Array[Double]], List[(Long, Array[Double])]](_.toList, { s: List[(Long, Array[Double])] =>
      val mh = new HashMap[Long, Array[Double]]
      s foreach { case (f, varr) => mh.update(f, varr) }
      mh
    })

  implicit def fmap2: Format[InducedFeatureMap] =
    wrap[InducedFeatureMap, HashMap[Long, Array[Double]]]((_.hmap.getOrElse(new HashMap[Long, Array[Double]])), { hm: HashMap[Long, Array[Double]] =>
      InducedFeatureMap(hm)
    })
}

abstract class CoreModelSerializer extends DefaultProtocol {
  import scala.collection.mutable.HashMap

  implicit def coreModelMap: Format[CoreModel] =
    asProduct3((a: Array[Double], b: Int, c: Int) => new CoreModel(a, b, c))((a: CoreModel) => (a.params, a.nfs, a.nls))

  implicit def mutableHashMap: Format[HashMap[String, FeatureType]] =
    wrap[HashMap[String, FeatureType], List[(String, FeatureType)]](_.toList, { (s: List[(String, FeatureType)]) =>
      val h = new HashMap[String, FeatureType]
      s foreach { case (k, v) => h += (k -> v) }
      h
    })
  implicit def featureTypeMap: Format[FeatureType] =
    asProduct5({ (a: Long, b: Boolean, c: Int, d: FeatureCat, e: Set[Feature]) =>
      val ft = new FeatureType(a, b, c, d)
      ft.set(e)
      ft
    })((ft: FeatureType) => (ft.fname, ft.edgep, ft.segsize, ft.fcat, ft.fdetail))

  implicit def featureMap: Format[Feature] =
    asProduct4((a: Int, b: Int, c: Int, d: Int) => new Feature(a, b, c, d))((f: Feature) => (f.prv, f.cur, f.fid, f.nfid))

  implicit def alphabetMap: Format[Alphabet[String]] =
    wrap[Alphabet[String], List[(String, Int)]](_.toList, { (s: List[(String, Int)]) =>
      val h = new Alphabet[String]
      s foreach { case (k, v) => h.update(k, v) }
      h
    })

  implicit def wordPropMap: Format[WordProperties] =
    wrap[WordProperties, List[(Long, List[String])]](_.toList, { (s: List[(Long, List[String])]) =>
      val h = new WordProperties
      s foreach { case (k, v) => h.update(k, v) }
      h
    })

  implicit def wordScoresMap: Format[WordScores] =
    wrap[WordScores, List[(Long, Double)]](_.toList, { (s: List[(Long, Double)]) =>
      val h = new WordScores
      s foreach { case (k, v) => h.update(k, v) }
      h
    })

  implicit def lexiconMap: Format[BloomLexicon] =
    wrap[BloomLexicon, List[(Long, BloomFilter)]]({ bl => bl.bloomTable.toList }, { (s: List[(Long, BloomFilter)]) =>
      val h = new BloomLexicon
      s foreach { case (k, v) => h.bloomTable += (k -> v) }
      h
    })

  implicit def bloomFilter: Format[BloomFilter] =
    asProduct4({ (k: Int, m: Int, n: Int, filter: collection.mutable.BitSet) =>
      val bf = new BloomFilter
      bf.size = k
      bf.width = m
      bf.nelements = n
      bf.filter = filter
      bf
    })((bf: BloomFilter) => (bf.size, bf.width, bf.nelements, bf.filter))

  implicit def bitsetMap: Format[collection.mutable.BitSet] =
    wrap[collection.mutable.BitSet, Array[Int]](_.toArray, { (ar: Array[Int]) =>
      val bs = new collection.mutable.BitSet(50000000)
      ar foreach { el => bs += el }
      bs
    })

  implicit def fcatMap: Format[FeatureCat] = asUnion[FeatureCat](NNFeature, MultiFeature, StdFeature)
  implicit def slabelMap: Format[SLabel] = wrap(_.v, SLabel)
  implicit def labelMap: Format[Label] = asProduct2(Label)(Label.unapply(_).get)
  implicit def beginMap: Format[BeginState] = wrap(_.s, BeginState)
  implicit def uncMap: Format[UncertainLabel] = wrap(_.labelString,{_:String => new UncertainLabel})

  implicit def abstractLabelMap: Format[AbstractLabel] = lazyFormat(asUnion[AbstractLabel](slabelMap, beginMap, labelMap, uncMap))

  implicit def abstractLabelAlphabetMap: Format[Alphabet[AbstractLabel]] =
    wrap[Alphabet[AbstractLabel], List[(AbstractLabel, Int)]](_.toList, { (s: List[(AbstractLabel, Int)]) =>
      val h = new Alphabet[AbstractLabel]
      s foreach { case (k, v) => h += (k -> v) }
      h
    })

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

object MaxEntSerializer extends CoreModelSerializer {
  import sbinary.Input._
  import sbinary.Output._

  import InducedFeatureMapProtocol._

  implicit def fsetMap: Format[Alphabet[Long]] =
    wrap[Alphabet[Long], List[(Long, Int)]](_.toList, { (s: List[(Long, Int)]) =>
      val h = new Alphabet[Long]
      s foreach { case (k, v) => h += (k -> v) }
      h
    })

  implicit def maxentModelMap: Format[MaxEntModel] =
    asProduct4((la: Alphabet[AbstractLabel], cm: CoreModel, fm: Alphabet[Long], indMap: Option[InducedFeatureMap]) => new MaxEntModel(la, cm, fm, indMap))((fm: MaxEntModel) => (fm.labelAlphabet, fm.crf, fm.fsetMap, fm.inducedMap))

  def writeModel(m: MaxEntModel, f: java.io.File) = {
    checkModel(m)
    val am = Model.compactMEModel(m)
    Operations.toFile[MaxEntModel](am)(f)
  }

  def serializeAsBytes(m: MaxEntModel) = Operations.toByteArray[MaxEntModel](m)
  def readModel(f: java.io.File): MaxEntModel = Operations.fromFile[MaxEntModel](f)
  def readModel(r: java.io.InputStream): MaxEntModel = Operations.read[MaxEntModel](r)
  def readModel(ba: Array[Byte]): MaxEntModel = Operations.fromByteArray[MaxEntModel](ba)
  def readModel(s: String): MaxEntModel = readModel(new java.io.File(s))

}

object NonFactoredSerializer extends CoreModelSerializer {
  import sbinary.Input._
  import sbinary.Output._

  class GetLongAlpha(val lb: collection.mutable.ListBuffer[(Long, Int)]) extends cern.colt.function.LongIntProcedure {
    def apply(fn: Long, i: Int) = {
      lb append ((fn, i))
      true
    }
  }

  implicit def longAlphabetMap: Format[LongAlphabet] = {
    wrap[LongAlphabet, List[(Long, Int)]](
      { case la : RandomLongAlphabet => List((0L,la.size))
        case la: LongAlphabet =>
        val olhmp = la.getUnderlyingMap
        val lb = new collection.mutable.ListBuffer[(Long, Int)]
        olhmp.forEachPair(new GetLongAlpha(lb))
        lb.toList
      }, { s: List[(Long, Int)] =>
        s match {
          case Nil =>
            new RandomLongAlphabet(115911564)
          case (_,sz) :: Nil => new RandomLongAlphabet(sz)
          case a => 
            val a = new LongAlphabet
            s foreach { case (l, i) => a.add(l, i) }
            a
        }
      })
  }

  
  implicit def nonfactoredModelMap: Format[NonFactoredModel] =
    asProduct7((fs: String, l: Option[BloomLexicon], wp: Option[WordProperties], s: Int, c: CoreModel, fm: LongAlphabet, ns: Int) =>
      new NonFactoredModel(fs, l, wp, s, c, fm, ns)){(m: NonFactoredModel) =>
        (m.fspec, m.lex, m.wp, m.segSize, m.crf, m.faMap, m.numStates)}

  def writeModel(m: NonFactoredModel, f: java.io.File) = Operations.toFile[NonFactoredModel](m)(f)
  def serializeAsBytes(m: NonFactoredModel) = Operations.toByteArray[NonFactoredModel](m)
  def readModel(f: java.io.File): NonFactoredModel = Operations.fromFile[NonFactoredModel](f)
  def readModel(r: java.io.InputStream): NonFactoredModel = Operations.read[NonFactoredModel](r)
  def readModel(ba: Array[Byte]): NonFactoredModel = Operations.fromByteArray[NonFactoredModel](ba)
  def readModelString(s: String): NonFactoredModel = readModel(s.getBytes)
  def readModel(s: String): NonFactoredModel = readModel(new java.io.File(s))
  def readModel(o: Option[String]): NonFactoredModel = o match {
    case Some(s) => readModel(s)
    case None => throw new RuntimeException("Model Not specified")
  }
  
  implicit def nonfactoredModelMapRand: Format[RandomNonFactoredModel] =
    asProduct7((fs: String, l: Option[BloomLexicon], wp: Option[WordProperties], s: Int, c: CoreModel, fm: RandomLongAlphabet, ns: Int) =>
      new RandomNonFactoredModel(fs, l, wp, s, c, fm, ns)){(m: RandomNonFactoredModel) =>
        val r : RandomLongAlphabet = m.faMap match {case m: RandomLongAlphabet => m case _ => new RandomLongAlphabet(1)} 
        (m.fspec, m.lex, m.wp, m.segSize, m.crf, r, m.numStates)}

  implicit def randomLongAlphabetMap: Format[RandomLongAlphabet] = {
    wrap[RandomLongAlphabet, Int](
      { rla: RandomLongAlphabet =>
        println("Writing out \"RandomLongAlphabet\" with integer: " + rla.size)
        rla.size }, { s: Int =>
          println("Reading in \"RandomLongAlphabet\" with integer: " + s)
          new RandomLongAlphabet(s) })
  }

  def writeRModel(m: RandomNonFactoredModel, f: java.io.File) = Operations.toFile[RandomNonFactoredModel](m)(f)
  def serializeAsBytes(m: RandomNonFactoredModel) = Operations.toByteArray[RandomNonFactoredModel](m)
  def readRModel(f: java.io.File): RandomNonFactoredModel = Operations.fromFile[RandomNonFactoredModel](f)
  def readRModel(r: java.io.InputStream): RandomNonFactoredModel = Operations.read[RandomNonFactoredModel](r)
  def readRModel(ba: Array[Byte]): RandomNonFactoredModel = Operations.fromByteArray[RandomNonFactoredModel](ba)
  def readRModelString(s: String): RandomNonFactoredModel = readRModel(s.getBytes)
  def readRModel(s: String): RandomNonFactoredModel = readRModel(new java.io.File(s))
  def readRModel(o: Option[String]): RandomNonFactoredModel = o match {
    case Some(s) => readRModel(s)
    case None => throw new RuntimeException("Model Not specified")
  }
  
}

object StandardSerializer extends CoreModelSerializer {
  import sbinary.Input._
  import sbinary.Output._
  import scala.collection.mutable.HashMap

  import InducedFeatureMapProtocol._

  class FTypeBuilder(val lbuf: collection.mutable.ListBuffer[(Long, FeatureType)]) extends cern.colt.function.LongObjectProcedure {
    def apply(k: Long, v: java.lang.Object) = {
      v match {
        case v: FeatureType => lbuf append ((k, v))
        case _ =>
      }
      true
    }
  }

  implicit def fsetMap: Format[OpenLongObjectHashMap] =
    wrap[OpenLongObjectHashMap, List[(Long, FeatureType)]]({ (fm: OpenLongObjectHashMap) =>
      val lbuf = new collection.mutable.ListBuffer[(Long, FeatureType)]
      fm.forEachPair(new FTypeBuilder(lbuf))
      lbuf.toList
    },
      { (s: List[(Long, FeatureType)]) =>
        val ht = new OpenLongObjectHashMap(s.length * 2, 0, 0.5)
        s foreach { case (i, ft) => ht.put(i, ft) }
        ht
      })

  implicit def auxMap: Format[ModelAuxiliaries] = asProduct4(ModelAuxiliaries)(ModelAuxiliaries.unapply(_).get)

  implicit def modelMap: Format[StdModel] =
    asProduct7((a: String, b: Boolean, c: ModelAuxiliaries, f: Int, g: Alphabet[AbstractLabel], h: CoreModel, i: OpenLongObjectHashMap) =>
      new StdModel(a, b, c, f, g, h, i))((a: StdModel) => (a.fspec, a.beg, a.aux, a.segSize, a.labelAlphabet, a.crf, a.fsetMap))

  def writeModel(m: StdModel, f: java.io.File) = {
    checkModel(m)
    val am: StdModel = Model.compactModel(m)
    Operations.toFile[StdModel](am)(f)
  }

  def serializeAsBytes(m: StdModel) = Operations.toByteArray[StdModel](m)
  def readModel(f: java.io.File): StdModel = Operations.fromFile[StdModel](f)
  def readModel(r: java.io.InputStream): StdModel = Operations.read[StdModel](r)
  def readModel(ba: Array[Byte]): StdModel = Operations.fromByteArray[StdModel](ba)
  def readModelString(s: String): StdModel = readModel(s.getBytes)
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
    model.fsetMap foreach {
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
