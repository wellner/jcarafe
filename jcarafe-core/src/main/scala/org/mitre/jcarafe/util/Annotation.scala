/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */
package org.mitre.jcarafe.util

class Annotation(val st: Int, val en: Int, var beg: Boolean, var typ: AbstractLabel, val vl: Option[String], val info: Option[Map[String,String]]) extends Ordered[Annotation] {
  def this(st: Int, en: Int, beg: Boolean, typ: AbstractLabel, vl: Option[String]) = this(st,en,beg,typ,vl,None)
  def this(st: Int, en: Int, beg: Boolean, typ: AbstractLabel) = this(st,en,beg,typ,None)
	
    def compare(that: Annotation) = 
      if (this.st < that.st) -1 
      else if (this.st > that.st) 1 
      else if (this.en < that.en) -1 
      else (typ,that.typ) match {
        case (SLabel(s),SLabel(s1)) => s.compare(s1)
        case (SLabel(_),Label(_,_)) => -1
        case (Label(_,_), SLabel(_)) => 1
        case _ => 0}
    
    def contained(that: Annotation) = (this.st >= that.st) && (this.en <= that.en)
    override def equals(other: Any): Boolean =
      other match {
        case that: Annotation => st == that.st && en == that.en && typ == that.typ && vl == that.vl
        case _ => false
      }
    override def hashCode : Int = 41 * (41 * (41 * ( 41 + st) + en) + typ.hashCode) + vl.hashCode
    override def toString() = {
      "start = " + st + " end = " + en + " type = " + typ + (if (beg) " beg=true" else " beg=false") + info 
    }
}	
