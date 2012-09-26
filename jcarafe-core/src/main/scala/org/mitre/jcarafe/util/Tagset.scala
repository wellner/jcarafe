package org.mitre.jcarafe.util

class Tagset(val set: Set[AbstractLabel]) {
  
  def isWithin(s: String) : Boolean = 
    set.exists{case e: Label => e.l == s case e: SLabel => e.v == s case _ => false}
  
  def attributesToStringSet(atts: Map[String,String]) : Set[(String,String)] = 
    atts.foldLeft(Set.empty:Set[(String,String)])((ac,e) => e match {case (k,v) => ac + ((k,v)) })
  
  def attributesToAttSet(atts: Map[String,String],keepit: Boolean) = 
    atts.foldLeft(Set.empty:Set[String]){(ac,e) => e match {case (k,v) => if ((v == "*") || keepit) ac + k else ac}}
  
  def isWithin(s: String, atts: Map[String,String]) : Boolean =
    getTag(s,atts) match {case Some(_) => true case None => false}
  
  def print() = set foreach {ab => println(ab.labelString)}
  
  override def toString() = {
    "'" + set.foldLeft("")(_ + _) + "'"
  }
  
  def labelMatch(s: String) : Boolean = set.exists {
  	case SLabel(l) => l == s
  	case Label(l,_) => l == s
  	case _ => false
  }	

  def isEmpty = set.isEmpty

  def getTag(s:String,atts: Map[String,String]) : Option[AbstractLabel] = {
    val aset = attributesToStringSet(atts)
    val attset = attributesToAttSet(atts,true)
    var t : Option[AbstractLabel] = None
    var newAtts : Map[String,String] = Map.empty
    set.foreach {
      case e: Label => 
        if (e.l == s) {
          if (attributesToStringSet(e.atts).subsetOf(aset))
            t = Some(e)
          else {
            val iset = attributesToAttSet(e.atts,false).intersect(attset)
            if (!iset.isEmpty) 
              t = Some(Label(e.l,atts.filter{case (k,v) => iset.contains(k)}))
          }}
      case e: SLabel => if (e.v == s) t = Some(e) 
      case _ => }
    t
  }
}

object Tagset {
    def parseTagSpec(s: String) : AbstractLabel = 
    s.split(":").toList match { 
      case Nil => throw new RuntimeException("Invalid Tag Specification")
      case h :: Nil => SLabel(h) 
      case h :: t =>
        val atts = 
          t map ((e:String) => e.split("=").toList match {
            case a :: v :: Nil => (a,v) 
            case _ => throw new RuntimeException("Invalid Tag Specification")}) 
        Label(h,(atts.foldLeft(Map():Map[String,String])(_ + _)))}
  
  def loadTagset(t: String)  = {
    val src = scala.io.Source.fromFile(new java.io.File(t))
    new Tagset(src.getLines().foldLeft(Set():Set[AbstractLabel]) {case (ac,l) => ac + parseTagSpec(l)}) 
  }

}
