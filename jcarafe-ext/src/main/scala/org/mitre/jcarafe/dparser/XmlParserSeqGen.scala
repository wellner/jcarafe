/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.dparser
import org.mitre.jcarafe.crf._
import scala.xml._
import scala.collection.mutable.ListBuffer
import org.mitre.jcarafe.util._

trait XmlParserSeqGen extends SeqGen[String] {
  type DeserializationT = XmlSeqDeserialization

  def createSource(l:Int,o:String, i: Map[String,String]) = {
    frep.createSource(l,o,true,Some(i))
  }
  
  implicit private def xmlAttsToSeq(x: MetaData) : Map[String,String] = 
    x.foldLeft(Map():Map[String,String])((ac,v) => ac + (v.key -> v.value.toString))
  
  implicit private def seqsToXmlAtts(x: Map[String,String]) = 
    x.foldLeft(Null: MetaData){(ac,t) => new UnprefixedAttribute(t._1,t._2,ac)}
  
  // nice and simple but inefficient with the flatMap rather than a fold
  private def getSequence(xml:NodeSeq) : Seq[Src] = xml flatMap {(s1:Node) =>
      s1 match {
        case Elem(_,"lex",a,_,c @ _*) =>
          val el = xmlAttsToSeq(a).get("gov") match { case Some(v) => v.toInt case None => 0 }
	  val s = createSource(el,c.text,a)
          List(s)
        case Elem(_,l,a,_,c @ _*) => getSequence(c)
        case a => getSequence(a.child)}}
  
  private def sourceSeqsOfDeserialized(tlists: NodeSeq) : Seqs = 
    tlists flatMap {(ns:Node) =>
      ns match {
        case Elem(_,l,_,_,c @ _*) if l == "s" => List(new SourceSequence(getSequence(c)))
        case a => sourceSeqsOfDeserialized(a.child)}}
        
  def deserializeFromFile(file: String) : DeserializationT = new XmlSeqDeserialization(scala.xml.XML.loadFile(file))	
  
  def deserializeFromString(string: String) : DeserializationT = new XmlSeqDeserialization(scala.xml.XML.load(new java.io.StringReader(string)))
  
  def deserializeFromRawString(s: String) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromRawString")

  def deserializeFromTokenSeq(s: Seq[String]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromRawString")
  
  def toSources(d: DeserializationT) = sourceSeqsOfDeserialized(d.ns)
  
  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) : Unit = 
    scala.xml.XML.save(f.getName,seqsToDeserialized(d,seqs).ns(0),"UTF-8")    
  
  def seqsToString(d: DeserializationT, seqs: Seq[InstanceSequence]) : String = 
    throw new RuntimeException("Unsupported method: seqsToString")	 

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = 
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Xml mode")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[InstanceSequence]) : Map[AbstractLabel,ListBuffer[Annotation]] =
	throw new RuntimeException("Unsupported method: seqsToAnnotations")
    
  def initialize() : Unit = throw new RuntimeException("Unsupported method: initialize")

  def seqsToAttributedDeserialization(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: seqsToAttributedDeserialization")
  
  /**
   * This Should actually work by having the decoder passed in to create seqs on the fly
   * This means the whole DOM traversal will be done once
   */
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = {
    var pos = 0
    var seqIndex = 0

    def addAnnotations(deserializedSeq: List[Node], seq: Seq[SeqElement]) : List[Node] = {
      deserializedSeq match {
        case (el @ Elem(p,"lex",atts,ns,c @ _*)) :: rest => 
          val ilab = seq(pos).label
          pos += 1
          new Elem(p,"lex",seqsToXmlAtts(atts + (("gov",ilab.toString))), ns, c:_*) :: addAnnotations(rest,seq)
        case Elem(p,l,atts,ns,c @ _*) :: rest =>
          val sub = addAnnotations(c.toList,seq)
          new Elem(p,l,atts,ns,sub:_*) :: addAnnotations(rest,seq)
        case n :: rest => n :: addAnnotations(rest,seq)
        case Nil => Nil
      }
    }
    
    def getAnnotationsOverSeq(xml_g: List[Node]) : List[Node] = xml_g match {
      case Nil => Nil
      case Elem(p,l,atts,ns,c @ _*) :: rest if l == "s" =>
        pos = 0 
        val seq = seqs(seqIndex).iseq
        val nannots = addAnnotations(c.toList,seq)
        seqIndex += 1
        new Elem(p,l,atts,ns,nannots:_*) :: getAnnotationsOverSeq(rest)
      case Elem(p,l,atts,ns,c @ _*) :: rest =>
        val subels = getAnnotationsOverSeq(c.toList)
        new Elem(p,l,atts,ns,subels:_*) :: getAnnotationsOverSeq(rest)
      case a :: rest => a :: getAnnotationsOverSeq(rest)
    }
    new XmlSeqDeserialization(getAnnotationsOverSeq(d.ns.toList))
  }
}
