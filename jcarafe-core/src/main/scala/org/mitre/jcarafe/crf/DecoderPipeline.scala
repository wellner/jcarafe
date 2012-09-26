package org.mitre.jcarafe.crf

import org.mitre.jcarafe.util.{ Options, Annotation }
import org.mitre.jcarafe.tokenizer.FastTokenizer
import java.io.InputStream

class DecoderPipeline(val preDecoders: List[StdDecoder], val decoder: StdDecoder) {

  def processString(s: String): String = {
    preDecoders match {
      case first :: rest =>
        val deserial = first.sGen.deserializeFromString(s)
        val srcs = first.decodeToSources(Set("lex"), "pre_", s)
        preDecoders.foreach { d => d.decodeSources(Set("lex"), "pre_", srcs) }
        decoder.decodeSeqsToString(s, srcs) // final "second stage" decoder
      case Nil => decoder.decodeString(s)
    }
  }

  def processStringList(seq: java.util.List[String]): java.util.List[String] = {
    decoder.decodeTokenSequenceToStatesJavaList(seq)
  }

  def processRawStringAsAnnotList(s: String): Array[Annotation] = {
    preDecoders match {
      case first :: rest =>
        val deserial = first.sGen.deserializeFromRawString(s)
        val srcs = first.decodeToSources(Set("lex"), "pre_", s)
        preDecoders.foreach { d => d.decodeSources(Set("lex"), "pre_", srcs) }
        decoder.decodeSeqsToAnnotations(s, srcs) // final "second stage" decoder
      case Nil => decoder.decodeStringToAnnotations(s)
    }
  }
}

object DecoderPipeline {

  def apply(args: Array[String]): DecoderPipeline = {
    apply(new Options(args))
  }
  
  def getJsonDecoder(modelFile: String) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("json")
    val decoder = getDecoder(opts, modelFile)
    new DecoderPipeline(Nil, decoder)
  }
  
  def getTextDecoder(modelFile: String) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("inline")
    val decoder = getDecoder(opts, modelFile)
    new DecoderPipeline(Nil, decoder)
  }
  
  def getBasicDecoder(modelFile: String) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("basic")
    val decoder = getDecoder(opts, modelFile)
    new DecoderPipeline(Nil, decoder)
  }
  
  def getJsonDecoder(modelStream: InputStream) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("json")
    val decoder = getDecoder(opts, "", Some(modelStream))
    new DecoderPipeline(Nil, decoder)
  }
  
  def getTextDecoder(modelStream: InputStream) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("inline")
    val decoder = getDecoder(opts, "", Some(modelStream))
    new DecoderPipeline(Nil, decoder)
  }
  
  def getBasicDecoder(modelStream: InputStream) : DecoderPipeline = {
    val opts = new Options
    opts.mode = Some("basic")
    val decoder = getDecoder(opts, "", Some(modelStream))
    new DecoderPipeline(Nil, decoder)
  }
  
  def apply(opts: Options): DecoderPipeline = {
    opts.checkRequired("--model", "--mode")
    val decoder = getDecoder(opts,opts.model.get)
    val preDecoders = opts.preModels map {m => getDecoder(opts,m)} 
    new DecoderPipeline(preDecoders, decoder)
  }
  
  def getDecoder(opts: Options, modelFile: String, stream: Option[java.io.InputStream] = None): StdDecoder = {
    val decoder = opts.mode match {
          case Some("inline") => new TextDecoder(opts,modelFile,stream)
          case Some("json") => new JsonDecoder(opts,modelFile,stream)
          case Some("basic") => new BasicDecoder(opts,modelFile,stream)
          case _ => throw new RuntimeException("Invalid or unspecified processing mode (choose \"inline\", \"json\" or \"basic\"")
      }
    decoder.setDecoder(true)
    decoder
  }

}