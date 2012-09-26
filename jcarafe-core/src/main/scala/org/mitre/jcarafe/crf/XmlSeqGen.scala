/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.xml._
import scala.collection.mutable.ListBuffer
import org.mitre.jcarafe.util._

class XmlSeqDeserialization(val ns: NodeSeq) extends Deserialization {
  type T = NodeSeq
  def getSlice(s:Int, e: Int) = this // not really implementable with XML representation
}

trait XmlConversions {
  implicit def xmlAttsToSeq(x: MetaData) : Map[String,String] = 
    x.foldLeft(Map():Map[String,String])((ac,v) => ac + (v.key -> v.value.toString))
  
  implicit def seqsToXmlAtts(x: Map[String,String]) = 
    x.foldLeft(Null: MetaData){(ac,t) => new UnprefixedAttribute(t._1,t._2,ac)}
  
  def printElemAsSingleTag(empty: Boolean, x: Node) = x match {
    case Elem(p,l,a,ns,c @ _*) =>
      val sb = new StringBuffer
      sb append "<"
      sb append l
      (a:Map[String,String]) foreach {case (k,v) => sb append " "; sb append k; sb append "=\""; sb append v; sb append "\""}
      if (empty) sb append "/"
      sb append ">"
      sb.toString
    case x => x.toString
    }
}

/**
 * Implements methods that facilitate generating sequences from inline XML representations
 * of data and annotations. This trait is to be mixed
 * into appropirate subclasses of <code>SeqGen</code> such as subclasses specialized for 
 * training or decoding.
 */
trait XmlSeqGen extends SeqGen[String] with FactoredSeqGen[String] with XmlConversions {

  type DeserializationT = XmlSeqDeserialization
  
  private def sourcesOfTokens(tlist:NodeSeq, l: AbstractLabel) : Seq[Src] = (tlist \\ "lex").toList match {
      case h :: t =>
        createSource(getState(l,addBeginStates),h.text,true,h.attributes) :: (t map ((tok:Node) => createSource(getState(l,false),tok.text,false,tok.attributes)))
      case Nil => Nil}

  // nice and simple but inefficient with the flatMap rather than a fold
  private def getSequence(xml:NodeSeq) : Seq[Src] = xml flatMap {(s1:Node) =>
      s1 match {
        case Elem(_,"lex",a,_,c @ _*) =>
          opts.tagset.getTag("lex",a) match {
            case Some(alabel) =>
              List(createSource(alabel,c.text,false,a))
            case None => List(createSource(SLabel("lex"),c.text,true,a))}
        case Elem(_,l,a,_,c @ _*) =>
          opts.tagset.getTag(l,a) match {
            case Some(alabel) => sourcesOfTokens(c,alabel)
            case _ => 
              if (l == "lex" || l == "LEX") List(createSource(SLabel("lex"),c.text,false,a))
              else getSequence(c)
          }
        case a =>
          getSequence(a.child)}}
  
  private def sourceSeqsOfDeserialized(tlists: NodeSeq) : Seqs = 
    tlists flatMap {(ns:Node) =>
      ns match {
        case Elem(_,l,_,_,c @ _*) if boundaries.labelMatch(l) =>
          List(createSourceSequence(getSequence(c)))
        case a => sourceSeqsOfDeserialized(a.child)}}
        
  def deserializeFromFile(file: String) : DeserializationT = new XmlSeqDeserialization(scala.xml.XML.loadFile(file))
  
  def deserializeFromString(string: String) : DeserializationT = new XmlSeqDeserialization(scala.xml.XML.load(new java.io.StringReader(string)))
  
  def seqsToAttributedDeserialization(d: DeserializationT, seqs: Seq[Seq[SeqElement]]) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: seqsToAttributedDeserialization")

  def deserializeFromRawString(s: String) : DeserializationT = 
    throw new RuntimeException("Unsuppoted method: deserializeFromRawString")
  
  def toSources(d: DeserializationT) = sourceSeqsOfDeserialized(d.ns)
  
  def seqsToFile(d: DeserializationT, seqs: Seq[Seq[SeqElement]], f: java.io.File) : Unit = 
    scala.xml.XML.save(f.getName,seqsToDeserialized(d,seqs).ns(0),"UTF-8")
  
  def seqsToString(d: DeserializationT, seqs: Seq[Seq[SeqElement]]) : String = 
    throw new RuntimeException("Unsupported method")	 

  def seqsToWriter(d: DeserializationT, seqs: Seq[Seq[SeqElement]], os: java.io.OutputStreamWriter) : Unit = 
    throw new RuntimeException("Seqs to Writer NOT SUPPORTED in Xml mode")
  
  def seqsToAnnotations(d:DeserializationT, seqs: Seq[Seq[SeqElement]]) : Map[AbstractLabel,ListBuffer[Annotation]] =
    throw new RuntimeException("Unsupported method: seqsToAnnotations")
  
  /*
   * This Should actually work by having the decoder passed in to create seqs on the fly
   * This means the whole DOM traversal will be done once
   */
  def seqsToDeserialized(d: DeserializationT, seqs: Seq[Seq[SeqElement]]) : DeserializationT = {
    val lexInd = lAlphabet.get(SLabel("lex")) match {case Some(v) => v case None => -1}
    var pos = 0
    var seqIndex = 0
    def addAnnotations(deserializedSeq: List[Node], seq: Seq[SeqElement]) : List[Node] = {
      deserializedSeq match {
        case all @ ((el @ Elem(p,"lex",atts,ns,c @ _*)) :: rest) => 
          val ilab = seq(pos).label
          val lab = invLa(ilab)
          if (ilab == lexInd) { 
            pos += 1
            el :: addAnnotations(rest,seq)
          } else lab match {
              case Label(t,aset) if (t == "lex") || (t == "LEX") => 
                pos += 1
                new Elem(p, t, aset, ns, c:_*) :: addAnnotations(rest,seq)
              case SLabel(t) =>
                if (t == "lex" || (t == "LEX")) {
                  pos += 1
                  new Elem(p, t, atts, ns, c:_*) :: addAnnotations(rest,seq)
                } else {
                  val (subels,newRest) = getRemainingTokens(seq,all,ilab,0,new ListBuffer())
                  if (!((t == "lex") || (t == "LEX")))
                    new Elem(p,t,Null,ns,subels:_*) :: addAnnotations(newRest,seq)
                  else subels ::: addAnnotations(newRest,seq)}
              case ILabel(i) => 
                val (subels,newRest) = getRemainingTokens(seq,all,ilab,0,new ListBuffer())
                subels ::: addAnnotations(newRest,seq)
              case BeginState(Label(t,aset)) =>                
                val nilab = lAlphabet.update(Label(t,aset))
                val (subels,newRest) = getRemainingTokens(seq,all,nilab,0,new ListBuffer())
                new Elem(p,t,aset,ns,subels:_*) :: addAnnotations(newRest,seq)
              case BeginState(SLabel(t)) =>
                val nilab = lAlphabet.update(SLabel(t))
                if (t == "lex" || (t == "LEX")) {
                  pos += 1
                  new Elem(p, t, atts, ns, c:_*) :: addAnnotations(rest,seq)
                } else {
                  val (subels,newRest) = getRemainingTokens(seq,all,nilab,0,new ListBuffer())
                  if (!((t == "lex") || (t == "LEX")))
                    new Elem(p,t,Null,ns,subels:_*) :: addAnnotations(newRest,seq)
                  else subels ::: addAnnotations(newRest,seq)}
              case BeginState(_) => throw new RuntimeException("Inadmissible label")
              case Label(t,aset) =>
                val (subels,newRest) = getRemainingTokens(seq,all,ilab,0,new ListBuffer())
                new Elem(p,t,aset,ns,subels:_*) :: addAnnotations(newRest,seq)
            }
        case Elem(p,l,atts,ns,c @ _*) :: rest =>
          val sub = addAnnotations(c.toList,seq)
          new Elem(p,l,atts,ns,sub:_*) :: addAnnotations(rest,seq)
        case n :: rest => n :: addAnnotations(rest,seq)
        case Nil => Nil
      }
    }
    
    def getRemainingTokens(seq: Seq[SeqElement], xml_g: List[Node], curInd: Int, lpos: Int, acc: ListBuffer[Node]) : Tuple2[List[Node],List[Node]] = {
      if (pos < seq.length) {
        val cli = seq(pos).label
        if (cli == curInd || lpos == 0) // always add the first token at lpos=0 
          xml_g match {
            case all @ ((a @ Elem(p,l,atts,ns,c @ _*)) :: rest) =>
              acc += a
              val nlpos = if ((l == "lex") || (l == "LEX")) { 
                pos += 1 
                lpos+1
              } else lpos 
              getRemainingTokens(seq,rest,curInd,nlpos,acc)
            case a :: rest =>
              acc += a
              getRemainingTokens(seq,rest,curInd,lpos,acc)
            case rest => (acc.toList,rest)
          }
        else (acc.toList,xml_g)
      } else (acc.toList,xml_g)
    }
    
    def getAnnotationsOverSeq(xml_g: List[Node]) : List[Node] = xml_g match {
      case Nil => Nil
      case Elem(p,l,atts,ns,c @ _*) :: rest if boundaries.labelMatch(l) =>
        pos = 0 
        val seq = seqs(seqIndex)
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
