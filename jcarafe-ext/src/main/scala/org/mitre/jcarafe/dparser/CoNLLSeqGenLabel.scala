package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{SourceSequence,ObsSource,InstanceSequence}
import org.mitre.jcarafe.util.SLabel
import collection.mutable.ListBuffer

trait CoNLLSeqGenLabel extends CoNLLSeqGen {
  
  override def toSources(d: DeserializationT): Seqs = {
    var sourceBuffer: ListBuffer[SourceSequence[String]] = new ListBuffer
    val splitChar = if (opts.useSpaces) ' ' else '\t'
    d.elements.foreach { seq =>
      val tmpBuf = new ListBuffer[ObsSource[String]]
      seq foreach { line =>
        line match {
          case id :: string :: _ :: pos :: _ :: _ :: gov :: label :: _ =>
            tmpBuf += createSource(new SLabel(label), string, Map("pos" -> pos, "gov" -> gov))
          case _ =>
        }
      }
      if (tmpBuf.size > 0) {
        val seq = tmpBuf.toIndexedSeq
        sourceBuffer += new SourceSequence(seq)
      }
    }
    sourceBuffer.toVector
  }

  override def seqsToDeserialized(d: DeserializationT, seqs: Seq[InstanceSequence]) : DeserializationT = {
    var i = 0
    val nd : Seq[Seq[List[String]]] = 
      d.elements map { seq => 
        var seqIndex = 0
        val iseq = seqs(i).iseq
        i += 1
        seq map { line => 
          line match {
            case id :: string :: x1 :: pos :: x2 :: x3 :: gov :: label :: r =>
              val nlab = invLa(iseq(seqIndex).label).labelHead
              seqIndex += 1
              id :: string :: x1 :: pos :: x2 :: x3 :: gov :: nlab :: r
            case r => r
          }
        }
      }
    new CoNLLDeserialization(nd)
  }

}