package org.mitre.jcarafe.crf

class DirectDecoder(model: StdModel) {

  import collection.JavaConverters._
  
  val sGen = new FactoredDecodingSeqGen[String](model) with DirectSeqGen
  lazy val invLa = sGen.invLa
  
  val viterbiDecoder: DecodingAlgorithm = Viterbi(false, 1, model.crf, false)
  
  private def decodeDeserialization(deser: DirectSeqDeserialization) = {
    val seqs = sGen.createSeqsWithInput(deser)
    seqs foreach viterbiDecoder.assignBestSequence
    seqs map {seq => seq.iseq map {ai => invLa(ai.label).labelString}}
  }
  
  def decodeLabeled(srcs: Seq[Seq[(String, Array[String])]]) : Seq[Seq[String]] = {
    decodeDeserialization(new DirectSeqDeserialization(srcs))
  }
  
  def decode(srcs: Seq[Seq[Array[String]]]) : Seq[Seq[String]] = {
    decodeDeserialization(DirectSeqDeserialization.buildFromUnlabeledInstances(srcs))
  }
  
  def decodeFromJavaLabeled(src: java.util.List[java.util.List[(String,java.util.List[String])]]) : java.util.List[java.util.List[String]] = {
    val in : Seq[Seq[(String,Array[String])]] = 
      src.asScala.toSeq map {s => 
        s.asScala.toSeq map {case (l, atts) => (l,atts.asScala.toArray)}}
    val res = decodeLabeled(in)
    val r1 = res map {s => s.asJava}
    r1.asJava
  }

  def decodeFromJava(src: java.util.List[java.util.List[java.util.List[String]]]) : java.util.List[java.util.List[String]] = {
    val in : Seq[Seq[Array[String]]] = 
      src.asScala.toSeq map {s => 
        s.asScala.toSeq map {case atts => atts.asScala.toArray}}
    val res = decode(in)
    val r1 = res map {s => s.asJava}
    r1.asJava
  }

}

object DirectDecoder {
  import StandardSerializer.readModel
  def apply(m: java.io.File) = new DirectDecoder(readModel(m))
}