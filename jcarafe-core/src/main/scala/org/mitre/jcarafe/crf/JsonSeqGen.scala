/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import org.mitre.jcarafe.tokenizer._
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import org.mitre.jcarafe.util._
import org.mitre.jcarafe.util.FastLoops._

class JsonSeqDeserialization(val json: JsonType) extends Deserialization {
  type T = JsonType
  def getSlice(s: Int, e: Int) = this // not really implementable with JSON representation
}

/**
 * Implements methods that facilitate generating sequences from standoff representations of
 * of data and annotations using a simple JSON-based encoding.  This trait is to be mixed
 * into appropriate subclasses of <code>SeqGen</code> such as subclasses specialized for
 * training or decoding.
 */
trait JsonSeqGen extends SeqGen[String] with FactoredSeqGen[String] {

  import org.mitre.jcarafe.util.JsonAnnotationHandler._

  type DeserializationT = JsonSeqDeserialization

  /*
   * Flag for whether this sequence generated is being run as a pre-process so
   * that output types should be used as attributes
   */
  val asPreProc = false

  def makeStackCurrent(st: Stack[Annotation], a: Annotation) = {
    var cur = false
    while (!cur) {
      if (st.isEmpty) cur = true
      else {
        val t = st.top
        if (t.en >= a.en) cur = true
        else st.pop
      }
    }
  }

  //ensure this gets tail-call optimized
  private final def popAnnotsToEnd(li: List[Annotation], pos: Int): List[Annotation] = li match {
    case h :: t => if (h.en <= pos) popAnnotsToEnd(t, pos) else h :: t
    case Nil => Nil
  }

  // merge annotations from s2 into s1 - copy attributes from annotations
  def mergeAnnotations(s1: List[Annotation], s2: List[Annotation]): List[Annotation] = {
    def mergeAnnotations_(s1: List[Annotation], s2: List[Annotation], acc: List[Annotation]): List[Annotation] =
      (s1, s2) match {
        case (a1 :: r1, a2 :: r2) if (a1.st == a2.st) =>
          val nmap: Map[String, String] = a1.info.getOrElse(Map()) ++ a2.info.getOrElse(Map())
          mergeAnnotations_(popAnnotsToEnd(r1, a2.en), r2, new Annotation(a2.st, a2.en, a2.beg, a1.typ, a1.vl, Some(nmap)) :: acc)
        case (a1 :: r1, a2 :: r2) if ((a1.st < a2.st)) => mergeAnnotations_(r1, a2 :: r2, a1 :: acc)
        case (a1 :: r1, a2 :: r2) if ((a2.st < a1.st)) => mergeAnnotations_(a1 :: r1, r2, acc)
        case (a1 :: r1, a2 :: r2) => mergeAnnotations_(r1, r2, a1 :: acc)
        case (a, Nil) => (a reverse_::: acc).reverse
        case (Nil, _) => Nil
      }
    mergeAnnotations_(s1, s2, Nil)
  }

  def tokenizeSignal(signal: String) = if (opts.rawDecode) FastTokenizer.parseStringNoTags(signal) else FastTokenizer.parseString(signal)
  //def tokenizeSignal(signal: String) = FastTokenizer.parseStringNoTags(signal)

  def makeBeginState(l: AbstractLabel): AbstractLabel = {
    //if (this.otherIndex < 0) this.otherIndex_=(this.lAlphabet.update(SLabel("lex"))) 
    if (this.lAlphabet.update(l) == this.otherIndex.getOrElse(-1)) l else BeginState(l)
  }

  def gatherAnnots(json: JsonType, opts: Options) = {
    val signal = json match {
      case JsObject(o) =>
        o("signal") match { case JsString(s) => s case _ => throw new RuntimeException("Expected signal to be a string") }
      case _ => throw new RuntimeException("No signal found")
    }
    var preProc = false
    val asets = json match { case JsObject(o) => try { o("asets") } catch { case _: Throwable => preProc = true; new JsArray(Nil) } case _ => preProc = true; new JsArray(Nil) }
    val tokSet = new Tagset(Set(SLabel("lex")))
    val zoneSet = if (opts.zoneset.isEmpty) new Tagset(Set(Label("zone", Map("region_type" -> "body")))) else opts.zoneset
    val existingTokens = getAnnotations(Some(signal), asets, tokSet, true, true).sortWith(_ < _) // get tokens with _any_ attributes they may have
    val tokenLabelDistributions = getAnnotations(Some(signal), asets, (new Tagset(Set(Label("tok_posterior_dist",Map())))), true, true).sortWith(_ < _)
    val zones = getAnnotations(None, asets, zoneSet) match {
      case Nil => List(new Annotation(0, signal.length, false, SLabel("zone"), None))
      case a => a
    }

    val preExistingTokens = if (tokenLabelDistributions.length > 1) tokenLabelDistributions else existingTokens
    val t_toks = if (opts.preProc || preProc) getTokensViaSignal(signal, zones) else sentenceSegmentTokenAnnotations(signal, preExistingTokens)
    val s_toks = t_toks.sortWith(_ < _) // sort ascending
    val toks = preExistingTokens match {
      case Nil => t_toks
      case ts => mergeAnnotations(s_toks, ts)
    }
    val targetAnnots = getAnnotations(None, asets, opts.tagset)
    val stack = new scala.collection.mutable.Stack[Annotation]
    stack pushAll (targetAnnots.sortWith(_ > _))
    (stack, toks.sortWith(_ < _), zones.sortWith(_ < _), signal)
  }

  val rr1 = """^[.?!]+[\"']*$""".r
  val wsrr = """^[ \n\r]+$""".r

  def sentenceSegmentTokenAnnotations(signal: String, toks: List[Annotation]) = {
    val sl = signal.length
    def segment(ts: List[Annotation]): Unit = ts match {
      case Nil =>
      case h :: t =>
        val hs = h.vl.getOrElse("")
        val after = if (h.en + 2 < sl) signal.substring(h.en + 1, h.en + 2) else ""
        if (rr1.findFirstIn(hs).isDefined && (wsrr.findFirstIn(after)).isDefined) {
          h.beg_=(true)
        }
        segment(t)
    }
    segment(toks)
    toks
  }

  def getTokensViaSignal(signal: String, zones: List[Annotation]): List[Annotation] = {
    val toks = new ListBuffer[Annotation]
    val signals: List[(String, Annotation)] = zones map { an => (signal.substring(an.st, an.en), an) }
    signals foreach {
      case (signal, an) =>
        val rawToks = tokenizeSignal(signal)
        val offset = an.st
        def gather(rts: List[Element], curPos: Int): Unit = {
          rts match {
            case Nil =>
            case h :: t =>
              val tok = h.getString
              val tlen = tok.length
              h :: t match {
                case HardEndTok(_) :: r =>
                  toks += new Annotation(curPos + offset, curPos + tlen + offset, true, SLabel("lex"), Some(signal.substring(curPos, curPos + tlen)), None)
                case SoftEndTok(_) :: EndWs(_) :: r =>
                  toks += new Annotation(curPos + offset, curPos + tlen + offset, true, SLabel("lex"), Some(signal.substring(curPos, curPos + tlen)), None)
                case SoftEndTok(_) :: Ws(_) :: r =>
                  toks += new Annotation(curPos + offset, curPos + tlen + offset, true, SLabel("lex"), Some(signal.substring(curPos, curPos + tlen)), None)
                case Ws(_) :: r =>
                case EndWs(_) :: r =>
                case Tag(_, _) :: r =>
                case Nil =>
                case a :: r =>
                  toks += new Annotation(curPos + offset, curPos + tlen + offset, false, SLabel("lex"), Some(signal.substring(curPos, curPos + tlen)))
              }
              gather(t, curPos + tlen)
          }
        }
        gather(rawToks, 0)
    }
    toks.toList
  }

  def deserializeFromFile(file: String): DeserializationT = new JsonSeqDeserialization(Json.constructJsonType(file))
  def deserializeFromString(string: String): DeserializationT = new JsonSeqDeserialization(Json.constructJsonTypeOfString(string))

  def deserializeFromTokenSeq(toks: Seq[String]): DeserializationT = {
    val sbuf = new StringBuilder()
    var lmSet = Set[(Int, Int)]()
    var offset = 0
    toks foreach { t => sbuf append t; sbuf append ' '; lmSet += ((offset, offset + t.length)); offset += (t.length + 1) }
    val lmList = lmSet.toList
    val js = JsObject(Map("signal" -> Json.constructJsonTypeOfRawString(sbuf.toString),
      "asets" -> JsArray(List(JsObject(Map("type" -> JsString("lex"),
        "attrs" -> JsArray(Nil),
        "annots" -> (JsArray(lmList map { case (s, e) => JsArray(List(JsInt(s), JsInt(e))) }))))))))
    new JsonSeqDeserialization(js)
  }

  def deserializeFromRawString(string: String): DeserializationT =
    new JsonSeqDeserialization(JsObject(Map("signal" -> Json.constructJsonTypeOfRawString(string))))

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) =
    Json.writeJson(seqsToDeserialized(d, seqs).json, f)

  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]) =
    Json.writeJsonToString(seqsToDeserialized(d, seqs).json)

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter, close: Boolean = true): Unit = {
    Json.writeJson(seqsToDeserialized(d, seqs).json, os, close)
  }
  val digits = "(\\p{Digit}+)"    
  
  def parseEncodedAbstractLabel(s: String) : AbstractLabel = {
    val Lab = """([A-z]+)\(([A-z]+),([A-z]+)\)""".r
    val Simple = """([A-z]+)""".r
    s match {
      case Lab(n,a,v) => Label(n,Map(a -> v))
      case Simple(s) => SLabel(s)
      case _ => throw new RuntimeException("Unparsable serialized label: " + s)
    }
  }

  def toSources(d: DeserializationT): Seqs = {
    val (stack, toks, zones, signal) = gatherAnnots(d.json, opts)
    def seqOfTokArr(tarr: Array[Annotation]) = {
      forIndex(tarr.length) { i =>
        val curTok = tarr(i)
        makeStackCurrent(stack, curTok)
        if (stack.isEmpty) curTok.typ_=(SLabel("lex"))
        else {
          val t = stack.top
          if ((curTok.contained(t) && (curTok.st == t.st)) || (curTok.st < t.st && curTok.en <= t.en && curTok.en > t.st)) {
            curTok.beg = true
            curTok.typ_=(t.typ)
          } else {
            curTok.typ_=(if (curTok.st >= t.st && curTok.st <= t.en) t.typ else SLabel("lex"))
          }
        }
      }
      Vector.tabulate(tarr.length) { (i: Int) =>
        val pt: Annotation = tarr(i)
        val obs = pt.vl match { case Some(s) => s case None => "" }
        val ainfo: Map[String, String] = Map("st" -> pt.st.toString, "en" -> pt.en.toString)
        val info = pt.info match { case Some(m) => ainfo ++ m case None => ainfo }

        if (addBeginStates && ((i > 0 && (!(pt.typ == SLabel("lex"))) && (!(tarr(i - 1).typ == pt.typ))) || i == 0 || pt.beg)) {
          createSource(getState(pt.typ, true), obs, pt.beg, info)
        } else {
          if (opts.empDistTrain) {
            // select elements from 'info' map that correspond to labels/states as specified with tagset
            val dist = info.toList.filter{case (l,s) =>
              val abLab = parseEncodedAbstractLabel(l)              
              opts.tagset.labelMatch(abLab.labelHead)}.map {case (l,s) => (parseEncodedAbstractLabel(l),s.toDouble)}

            createDistributionalSource(dist,"",true,Map())            
          } else createSource(pt.typ, obs, pt.beg, info)
        }
      }
    }

    def segment(curSegs: List[List[Annotation]], zones: List[Annotation], toks: List[Annotation]): List[List[Annotation]] =
      (curSegs, zones, toks) match {
        case (s :: r, z1 :: zr, t1 :: tr) =>
          if (t1.st < z1.st) segment(s :: r, z1 :: zr, tr)
          else if (!opts.noSentBreak && t1.beg) segment(Nil :: (t1 :: s).reverse :: r, z1 :: zr, tr)
          else if (t1.contained(z1)) segment((t1 :: s) :: r, zones, tr)
          else {
            zr match {
              case z2 :: zrr =>
                if (t1.contained(z2)) segment(List(t1) :: s.reverse :: r, zr, tr)
                else if (t1.st < z2.st) segment(Nil :: s.reverse :: r, z2 :: zrr, tr)
                else segment(Nil :: s.reverse :: r, zrr, tr)
              case Nil => s.reverse :: r
            }
          }
        case (s :: r, _, _) => s.reverse :: r
        case _ => Nil
      }
    val tokLists = segment(List(List()), zones.sortWith(_ < _), toks)

    tokLists.reverse map { el: List[Annotation] =>
      val sss = seqOfTokArr(el.toArray).toSeq
      createSourceSequence(sss)
    }
  }

  def getAState(l: AbstractLabel, v: Boolean) = if (addBeginStates) getState(l, v) else l

  val seqConfidenceAnnotationType = Label("seq_confidence", Map("posterior" -> ""))
  val tokConfidenceAnnotationType = Label("tok_confidence", Map("posterior" -> "", "entropy" -> ""))
  lazy val tokPosteriorAnnotationType = Label("tok_posterior_dist", lAlphabet.foldLeft(Map[String, String]()) { case (ac, (l, i)) => ac + (l.labelString -> "") })
  val logVal2 = math.log(2.0)
  def log2(x: Double) = math.log(x) / logVal2

  private def getEntropy(map: Map[Int, Double]) = {
    var e = 0.0
    map foreach { case (i, v) => e -= log2(v) * v }
    e
  }

  private def addTokenDistAnnotation(atbl: Map[AbstractLabel, ListBuffer[Annotation]], cp: AbstractInstance, st: Int, en: Int) = {
    var annotTbl = atbl
    val tokPosteriorMap = lAlphabet.foldLeft(Map(): Map[String, String]) { case (ac, (lab, i)) => ac + (lab.labelString -> cp.conditionalProb(i).toString) }
    val tokPosteriors = new Annotation(st, en, false, Label("tok_posterior_dist", tokPosteriorMap), None)
    if (!annotTbl.contains(tokPosteriorAnnotationType)) annotTbl = annotTbl + (tokPosteriorAnnotationType -> new ListBuffer[Annotation])
    annotTbl(tokPosteriorAnnotationType) += tokPosteriors
    annotTbl
  }
  
  private def getStartEnd(info: Option[Map[String,String]]) = {
    info match {case Some(i) => (i("st").toInt, i("en").toInt) case None => (-1,-1)}
  }

  def seqsToAnnotations(d: DeserializationT, seqs: Seq[InstanceSequence]): scala.collection.immutable.Map[AbstractLabel, ListBuffer[Annotation]] = {
    if (addBeginStates) StateCache.updateStateCache(lAlphabet) // make sure the state cache is updated for handling BEGIN states
    val asets = d.json match { case JsObject(o) => try { o("asets") } catch { case _: Throwable => new JsArray(Nil) } case _ => new JsArray(Nil) }
    var annotTbl = scala.collection.immutable.Map[AbstractLabel, ListBuffer[Annotation]]()
    val pairs = toSources(d)
    for (i <- 0 until seqs.length) {
      val seq = seqs(i).iseq
      if (seq.length > 0) {
        val rawPairs = pairs(i)
        if (opts.confidences) {
          val seqStart = rawPairs(0).info match { case Some(am) => am("st").toInt case None => -1 }
          val seqEnd = rawPairs(0 max (seq.length - 1)).info match { case Some(am) => am("en").toInt case None => -1 }
          val nannot = new Annotation(seqStart, seqEnd, false, Label("seq_confidence", Map("posterior" -> seqs(i).seqPosteriorProbability.toString)), None)
          if (!annotTbl.contains(seqConfidenceAnnotationType)) annotTbl = annotTbl + (seqConfidenceAnnotationType -> new ListBuffer[Annotation])
          annotTbl(seqConfidenceAnnotationType) += nannot
        }
        var c = 0
        while (c < seq.length) {
          val ilab = seq(c).label
          val lab = invLa(ilab)
          val nlabState = lab match { case BeginState(l) => l case a => a }
          val normLab_c = lAlphabet.update(nlabState)
          val normLab = if (normLab_c < 0) ilab else normLab_c   
          val lstr = lab.labelString
          var st = -1
          var en = -1
          val (s,e) = getStartEnd(rawPairs(c).info)
          st = s
          en = e
          if (opts.confidences) {
            //var entropy = 0.0
            val tokEntropy = getEntropy(seq(c).getCondProbTable)
            val tokConfidence = new Annotation(st, en, false,
              Label("tok_confidence",
                Map("posterior" -> seq(c).conditionalProb(ilab).toString,
                  "entropy" -> tokEntropy.toString)), None)
            if (!annotTbl.contains(tokConfidenceAnnotationType)) annotTbl = annotTbl + (tokConfidenceAnnotationType -> new ListBuffer[Annotation])
            annotTbl(tokConfidenceAnnotationType) += tokConfidence
          } else if (opts.posteriors) {
            val cp = seq(c)
            annotTbl ++= addTokenDistAnnotation(annotTbl,cp,st,en)
          }
          if (!(lstr == "lex")) { // case where we output an identified phrase
            st = rawPairs(c).info match { case Some(amap) => amap("st").toInt case None => st } // start of annotation
            var curLab = normLab
            do {
              c += 1
              if (c < seq.length) {
                curLab = seq(c).label
                val (s,e) = getStartEnd(rawPairs(c).info)
                val cp = seq(c)
                if (opts.posteriors) annotTbl ++= addTokenDistAnnotation(annotTbl,cp,s,e)                
              }
            } while ((curLab == normLab) && (c < seq.length))            
             // advance counter to the end of the phrase              
            en = rawPairs(c - 1).info match { case Some(amap) => amap("en").toInt case None => (-1) }
            val annot = new Annotation(st, en, false, nlabState, None)
            val stateIndex = nlabState match {
              case Label(s, atts) =>
                Label(s, atts.foldLeft(Map(): Map[String, String]) { case (ac, (k, v)) => ac + (k -> "") }) case a => a
            }
            if (annotTbl.contains(stateIndex))
              annotTbl(stateIndex) += annot
            else {
              val n = new ListBuffer[Annotation]
              n += annot
              annotTbl = annotTbl + (stateIndex -> n)
            }
          } else c += 1
        }
      }
    }
    annotTbl
  }

  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]): DeserializationT = {
    val annotTbl = seqsToAnnotations(d, seqs)
    val jsonPhrases: List[JsonType] =
      (annotTbl map {
        case (atyp, annots) =>
          atyp match {
            case SLabel(s) =>
              JsObject(Map("type" -> JsString(s), "attrs" -> JsArray(Nil),
                "annots" -> (JsArray(annots.toList map { a: Annotation => JsArray(List(JsInt(a.st.toInt), JsInt(a.en.toInt))) }))))
            case Label(s, atts) =>
              val keys = (atts map { case (k, v) => k }).toList
              JsObject(Map("type" -> JsString(s), "attrs" -> JsArray(keys map { JsString(_) }),
                "annots" -> (JsArray(annots.toList map { a: Annotation =>
                  a.typ match {
                    case Label(_, atts) =>
                      val attvals = keys map { k => JsString(atts(k)) }
                      JsArray(JsInt(a.st.toInt) :: JsInt(a.en.toInt) :: attvals)
                    case _ => throw new RuntimeException("Incompatible annotation")
                  }
                }))))
            case _ => throw new RuntimeException("Incompatible Annotation")
          }
      }).toList
    val jsonObj = d.json match {
      case JsObject(obj) =>
        val a1 =
          (try { obj("asets") match { case JsArray(a) => a case _ => throw new RuntimeException("Invalid obj") } }
          catch { case e: java.util.NoSuchElementException => Nil case e: Throwable => throw e })
        JsObject(obj.updated("asets", JsArray(jsonPhrases ::: a1)))
      case a => a
    }
    new JsonSeqDeserialization(jsonObj)
  }
}	
