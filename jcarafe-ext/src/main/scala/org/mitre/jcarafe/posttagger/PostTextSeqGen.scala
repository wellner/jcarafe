/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger

import scala.collection.mutable.ListBuffer
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._
import org.mitre.jcarafe.tokenizer._

trait PostTextSeqGen extends FactoredSeqGen[Array[String]] with XmlConversions {

  type DeserializationT = TextSeqDeserialization

  def deserializeFromFile(file: String) : DeserializationT = new TextSeqDeserialization(FastTokenizer.parseFile(file))
  
  def deserializeFromString(s: String) : DeserializationT = new TextSeqDeserialization(FastTokenizer.parseString(s))
  
  def constructTag(t: String) = {
    val sb = new StringBuffer
    sb append "<"; 
    if ((t.length > 2) && (t(t.length-2) == '/')) sb append t.substring(1,t.length-2)
    else sb append t.substring(1,t.length-1)
    sb append "/>";
    scala.xml.XML.loadString(sb.toString)
  }
  
  def getLabelAndAttrsFromTag(t: String) : (String,Map[String,String]) = {
     constructTag(t) match {
      case scala.xml.Elem(_,l,a,_,_*) => (l,a)
      case _ => throw new RuntimeException("Invalid XML element")
      }
    }
  
  def deserializeFromRawString(s: String) : DeserializationT = deserializeFromString(s)

  def deserializeFromTokenSeq(s: Seq[String]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromTokenSeq")
  
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT =
    throw new RuntimeException("This method unsupported for antecedent mode")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[InstanceSequence]) : Map[AbstractLabel,ListBuffer[Annotation]] =
	throw new RuntimeException("Unsupported method: seqsToAnnotations")
  
  def seqsToString(d:DeserializationT, seqs: Seq[InstanceSequence]) : String = 
    throw new RuntimeException("Unsupported method")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) : Unit =
  	seqsToStream(d, seqs, new java.io.FileOutputStream(f))

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit

  def seqsToStream(d: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream) : Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    seqsToWriter(d,seqs,os)
    os.flush
    os.close
    ostr.close
  } 

  def toSources(d: DeserializationT) : Seqs = {
    var state : AbstractLabel = SLabel("Post")
    var curAtts : Map[String,String] = Map.empty 
    val tbuf = new ListBuffer[ObsSource[Array[String]]]
    val tokBuf = new ListBuffer[String] 
    d.elements foreach {
        case Tag(t,true) =>
          try {
            val (l,attmap) = getLabelAndAttrsFromTag(t) // parse tag to get label and attribute value pairs
              opts.tagset.getTag(l,attmap) match {
                case Some(lab) =>
                  state = lab
                  curAtts = attmap
                case None =>
              }
          } catch { case _ => println("Warning: tag " + t + " not parsed correctly")} // always continue if tag doesn't parse
        case t @ (HardEndTok(_) | SoftEndTok(_) | Tok(_)) => tokBuf += t.getString
        case Tag(t,false) if t.startsWith("</Post") => 
          val tarr = tokBuf.toArray
          tbuf += createSource(state,tokBuf.toArray,curAtts)
	  tokBuf.clear
          state = SLabel("Post")
        case _ => 
    }
    val tb = tbuf.toIndexedSeq
    List(new SourceSequence(tbuf.toSeq))
  }
}
