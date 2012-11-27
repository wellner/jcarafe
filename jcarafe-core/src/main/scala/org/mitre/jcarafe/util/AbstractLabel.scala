package org.mitre.jcarafe.util

sealed abstract class AbstractLabel extends java.io.Serializable {
  val labelString : String
  def labelTag(b: Boolean) : String
  def assoc(s: String) : String
  val labelHead : String
  override def equals(other: Any): Boolean =
    other match {
      case that: AbstractLabel => labelString == that.labelString
      case _ => false
    }
  override def hashCode : Int = labelString.hashCode
  override def toString = labelString
  def hasValue(s: String) : Option[(String,String)] = None
  def hasValue(sl: List[String]) : Option[(String,String)] = None
  def hasAttributeWithWildCard(s: String) : Option[(String,String)] = None
}

case class ILabel(val v: Int) extends AbstractLabel {
  val labelString = v.toString
  val labelHead = v.toString
  def labelTag(b:Boolean) = v.toString
  val label = v
  def assoc(s: String) = throw new RuntimeException("Assoc not possible with ILabel")
}

case class SLabel(val v: String) extends AbstractLabel {
  val labelString = v
  val labelHead = v
  def assoc(s: String) = throw new RuntimeException("Assoc not possible with Simple Label")
  def labelTag(closed: Boolean) : String = {
    val s = new StringBuilder
    if (closed) s append "</" else s append "<" 
    s append v; 
    s append ">"
    s.toString
  }
}
case class Label(val l: String, val atts: Map[String,String]) extends AbstractLabel {
  val labelString = l + atts.foldLeft("")(_ + _)
  val labelHead = l
  def assoc(s: String) = atts(s)
  def labelTag(closed: Boolean) = { 
    val s = new StringBuilder
    if (closed) s append "</" else s append "<" 
    s append l
    if (!(closed)) {atts foreach {case (k,v) => s append " "; s append k; s append "=\""; s append v; s append "\"" }}
    s append ">"
    s.toString
  }
  override def hasValue(s: String) : Option[(String,String)] = {
    val v = atts.find {case (_,v) => v == s}
    println("Checking whether label " + this + " has value = " + s)
    v
  }
  override def hasValue(sl: List[String]) : Option[(String,String)] = {
	  sl.foldLeft(None: Option[(String,String)]) {(ac,v) => hasValue(v)}
  }
  override def hasAttributeWithWildCard(s: String) : Option[(String,String)] = {
    atts.find {case (a,v) => a == s && v == "*"}
  }
} 

case class BeginState(val s: AbstractLabel) extends AbstractLabel {
  val labelString = "B:" + s.labelString
  val labelHead = s.labelHead
  def labelTag(b: Boolean) = s.labelTag(b)
  def unbegin = s
  def assoc(sv: String) = s.assoc(sv)
}
