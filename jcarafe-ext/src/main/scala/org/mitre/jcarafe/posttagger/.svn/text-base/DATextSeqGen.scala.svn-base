
package org.mitre.jcarafe.posttagger

import org.mitre.jcarafe.crf.{InstanceSequence}
import org.mitre.jcarafe.util.{Label,SLabel}
import org.mitre.jcarafe.tokenizer.{Tag}


trait DATextSeqGen extends PostTextSeqGen {

  def seqsToWriter(d: DeserializationT, seqs: Seq[InstanceSequence], os: java.io.OutputStreamWriter) : Unit = {
    assert(seqs.length == 1) // this assumes there is a single sequence for each deserialization    
    val flatSeqs = seqs(0).iseq.toArray
    var i = 0
    d.elements foreach {
      case Tag(t,true) if t(1) != '?' => 
        val (l,attmap) = getLabelAndAttrsFromTag(t)
        if (l == "Post") { 
            val curId = attmap("id").toInt
            val labStr = invLa(flatSeqs(i).label) match {case Label(l,a) => a("dialog-act") case _ => "unk"}
            val attStr = attmap.foldLeft(""){case (ac,(k,v)) => ac + " " + k + "=\"" + v + "\""} 
            os.write("<")
            os.write(l)
            os.write(" dialog-act=\"")
            os.write(labStr)
            os.write("\"")
            os.write(attStr)
            os.write(">")
            i += 1
        }
      case Tag(t,false) => os.write(t)
      case t => os.write(t.getString)
    }
  }


}
