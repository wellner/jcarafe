
package org.mitre.jcarafe.posttagger

import org.mitre.jcarafe.crf.{InstanceSequence}
import org.mitre.jcarafe.util.{Label,SLabel}
import org.mitre.jcarafe.tokenizer.{Tag}


trait SummarizationTextSeqGen extends PostTextSeqGen {

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = {
    assert(seqs.length == 1) // this assumes there is a single sequence for each deserialization    
    val flatSeqs = seqs(0).iseq.toArray
    println("length d = " + d.elements.length)
    println("length seq = " + flatSeqs.length)
    var i = 0
    d.elements foreach {
      case Tag(t,true) if t(1) != '?' => 
        val (l,attmap) = getLabelAndAttrsFromTag(t)
        if (l == "Post") { 
            val labStr = invLa(flatSeqs(i).label) match {case Label(l,a) => a("summary") case _ => "unk"}
            val attStr = attmap.foldLeft(""){case (ac,(k,v)) => ac + " " + k + "=\"" + v + "\""} 
            os.write("<")
            os.write(l)
            os.write(" summary=\"")
            os.write(labStr)
            os.write("\"")
            os.write(attStr)
            os.write(">")
	    println("i = " + i + " and attstr = " + attStr)
            i += 1
        }
      case Tag(t,false) => os.write(t)
      case t => os.write(t.getString)
    }
  }


}
