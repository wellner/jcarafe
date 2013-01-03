/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._
import org.mitre.jcarafe.tokenizer._
import org.mitre.jcarafe.tokenizer.FastTokenizer
import scala.collection.mutable.ListBuffer
import scala.xml._

trait AntecedentSeqGen extends SeqGen[Array[PostTok]] with XmlConversions {
  type DeserializationT = TextSeqDeserialization
  
  def deserializeFromFile(file: String) : DeserializationT = new TextSeqDeserialization(FastTokenizer.parseFile(file))
  
  def deserializeFromString(s: String) : DeserializationT = new TextSeqDeserialization(FastTokenizer.parseString(s))

  def deserializeFromTokenSeq(s: Seq[String]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromTokenSeq")
  
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
  
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT =
    throw new RuntimeException("This method unsupported for antecedent mode")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[InstanceSequence]) : Map[AbstractLabel,ListBuffer[Annotation]] =
	throw new RuntimeException("Unsupported method: seqsToAnnotations")

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = 
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Xml mode")
  
  def seqsToString(d:DeserializationT, seqs: Seq[InstanceSequence]) : String = 
    throw new RuntimeException("Unsupported method")

  def seqsToFile(d: DeserializationT, seqs: Seq[InstanceSequence], f: java.io.File) : Unit =
  	seqsToStream(d, seqs, new java.io.FileOutputStream(f))
  
  def seqsToStream(d: DeserializationT, seqs: Seq[InstanceSequence], ostr: java.io.OutputStream) : Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val flatSeqs = seqs(0).iseq.toArray
    assert(seqs.length == 1) // this assumes there is a single sequence for each deserialization
    var i = 0
    d.elements foreach {
      case Tag(t,true) if t(1) != '?' => 
        val (l,attmap) = getLabelAndAttrsFromTag(t)
        opts.tagset.getTag(l,attmap) match {
          case Some(_) =>
            val lab = flatSeqs(i).label
            val attStr = attmap.foldLeft(""){case (ac,(k,v)) => ac + " " + k + "=\"" + v + "\""} 
            os.write("<")
            os.write(l)
            os.write(" antecedent-distance=\"")
            os.write(lab.toString)
            os.write("\"")
            os.write(attStr)
            os.write(">")
            i += 1
          case None => os.write(t)
        }
      case Tag(t,false) => os.write(t)
      case t => os.write(t.getString)
    }
    os.flush
  } 
    

  def toSources(d: DeserializationT) : Seqs = {
    var state = -1
    var curAtts : Map[String,String] = Map.empty 
    val tbuf = new ListBuffer[ObsSource[Array[PostTok]]]
    val tokBuf = new ListBuffer[PostTok] 
    d.elements foreach {
        case Tag(t,true) if (t(1) != '?') =>	
          try {
            val (l,attmap) = getLabelAndAttrsFromTag(t) // parse tag to get label and attribute value pairs
            opts.tagset.getTag(l,attmap) match {
              case Some(_) => 
                  val utteranceId = attmap("id").toInt
                  val antecedentId = attmap.get("antecedent-distance") match {case Some(v) => v.toInt case None => 0}
                  curAtts = curAtts + ("user" -> attmap("user"))
                  attmap.get("dialog-act") match { case Some(d) => curAtts = curAtts + ("dialog-act" -> d) case None => }
                  state = antecedentId
                case None =>
              }
          } catch { case _ => println("Warning: tag " + t + " not parsed correctly")} // always continue if tag doesn't parse
        case t @ (HardEndTok(_) | SoftEndTok(_) | Tok(_)) => tokBuf += new PostTok(t.getString)
        case Tag(t,false) => {tbuf += createSource(ILabel(state),tokBuf.toArray, curAtts); tokBuf.clear; state = 0}
        case _ => 
      }
    List(new SourceSequence(tbuf.toSeq))
  }
      
}
