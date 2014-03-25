/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import org.mitre.jcarafe.crf.{JsonSeqDeserialization,SeqGen,SourceSequence,InstanceSequence}
import org.mitre.jcarafe.tokenizer._
import org.mitre.jcarafe.util._
import scala.collection.mutable.ArrayBuffer

trait PostJsonSeqGen extends SeqGen[Array[PostTok]] {
  type DeserializationT = JsonSeqDeserialization

  import org.mitre.jcarafe.util.JsonAnnotationHandler._

  private def tokenizeSignal(signal: String) : Seq[Element] = FastTokenizer.parseString(signal)

  def deserializeFromFile(file: String) : DeserializationT = new JsonSeqDeserialization(Json.constructJsonType(file))
  def deserializeFromString(string: String) : DeserializationT = new JsonSeqDeserialization(Json.constructJsonTypeOfString(string))

  def deserializeFromTokenSeq(s: Seq[String]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromTokenSeq")

  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]) = 
    Json.writeJsonToString(seqsToDeserialized(d,seqs).json)

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) = 
    Json.writeJson(seqsToDeserialized(d,seqs).json,f)
  
  def toSources(d: DeserializationT) : Seqs = {
    val json = d.json
    val asets = json match {case JsObject(o) => try {o("asets")} catch {case _:Throwable => new JsArray(Nil)} case _ =>  new JsArray(Nil)}
    val signal = json match { 
      case JsObject(o) => 
        o("signal") match {case JsString(s) => s case _ => throw new RuntimeException("Expected signal to be a string")}
      case _ => throw new RuntimeException("No signal found")}
    val targetAnnots = getAnnotations(None,asets,opts.tagset)
    val seq = targetAnnots map {an: Annotation => 
      val toks : Seq[Element] = tokenizeSignal(signal.substring(an.st, an.en))
      val tbuf = new ArrayBuffer[PostTok]
      toks foreach {case Tok(t) => tbuf += new PostTok(t) case _ => }				
      createSource(an.typ,tbuf.toArray)}
    Vector(new SourceSequence(seq))
  }

  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = {
    val json = d.json
    val asets = json match {case JsObject(o) => try {o("asets")} catch {case _: Throwable => new JsArray(Nil)} case _ =>  new JsArray(Nil)}
    val seq = seqs(0) // only a single sequence per deserialization
    val targetAnnots : List[(Annotation,Int)] = getAnnotations(None,asets,opts.tagset,true).sortWith(_ < _).zipWithIndex
    var keys : Set[String] = Set()
    targetAnnots match {case (a,_) :: _ => a.info.getOrElse(Map()) foreach {case (k,v) => keys += k} case Nil => }
    val nannots : List[JsonType] = targetAnnots map {      
      case (an,i) => 
	val attvals = an.info match {case None => Nil case Some(m) => 
	  m.toList map {case (k,v) => JsString(v)}}
	JsArray(JsInt(an.st) :: JsInt(an.en) :: JsString(seq.iseq(i).label.toString) :: attvals)}
    val keyAtts = JsString("antecedent-distance") :: (keys.toList map {JsString(_)})
    val naset = JsObject(Map("type" -> JsString("Post"), "attrs" -> JsArray(keyAtts), "annots" -> JsArray(nannots)))
    val jsObj = json match {
      case JsObject(obj) => JsObject(obj.updated("asets",JsArray(List(naset))))
      case a => a}
    new JsonSeqDeserialization(jsObj)
  }

}

