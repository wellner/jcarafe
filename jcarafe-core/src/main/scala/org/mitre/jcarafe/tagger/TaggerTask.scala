/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.tagger

import org.mitre.jcarafe.crf.{
  Trainer,
  Decoder,
  FactoredDecodingSeqGen,
  TextSeqGen,
  JsonSeqGen,
  BasicSeqGen,
  SeqGenScorer,
  StreamingDecoder,
  StdDecoder,
  StdTrainer,
  TrainingSeqGen,
  TextSeqDeserialization,
  InducedFeatureMap
}
import org.mitre.jcarafe.util.Options
import org.mitre.jcarafe.tokenizer.FastTokenizer
import collection.mutable.HashMap

abstract class Mode
case object Text extends Mode
case object Json extends Mode
case object Basic extends Mode

abstract class TaggerTask[Obs](argv: Array[String]) {

  val trainer: Trainer[Obs]
  val decoder: Decoder[Obs]

  lazy val opts = new Options(argv)

  def process() = {
    if (opts.train) {
      opts.diskCache match { case Some(d) => if (opts.train) org.mitre.jcarafe.crf.CrfInstance.diskCache = Some(d) case None => }
      trainer.train()
    } else {
      decoder.setDecoder(true) // ensure decoder is set
      val eval = opts.evaluate match { case Some(_) => true case None => false }
      decoder.decode()
      decoder.cleanUp()
      if (eval) println("Token(element)-level accuracy: " + decoder.sGen.getAccuracy)
    }
  }
}

class StdTaggerTask(val opts: Options) {

  def this(argv: Array[String]) = this(new Options(argv))

  val mode = opts.mode match {
    case Some("inline") => Text case Some("json") => Json case Some("basic") => Basic
    case None => Text
  }

  if (opts.train) org.mitre.jcarafe.crf.CrfInstance.training = true
  if (opts.noCache) org.mitre.jcarafe.crf.CrfInstance.useCache = false
  opts.diskCache match { case Some(d) => if (opts.train) org.mitre.jcarafe.crf.CrfInstance.diskCache = Some(d) case None => }
  opts.tokenizerPatterns foreach { f =>
    org.mitre.jcarafe.tokenizer.FastTokenizer.setTokenizerAugmenters(new java.io.File(f))
  }

  def getDecoder(modelFile: String, eval: Boolean): StdDecoder = {
    val decoder =
      new StdDecoder(opts, modelFile) {
        val sGen = mode match {
          case Text =>
            if (eval) new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen with SeqGenScorer[String]
            else if (opts.rawDecode)
              new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen with StreamingDecoder {
                override def deserializeFromFile(f: String) = new TextSeqDeserialization(FastTokenizer.parseFileNoTags(f))
              }
            else if (opts.streaming)
              new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen with StreamingDecoder
            else new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen
          case Json =>
            if (eval) new FactoredDecodingSeqGen[String](model, opts) with JsonSeqGen with SeqGenScorer[String]
            else new FactoredDecodingSeqGen[String](model, opts) with JsonSeqGen
          case Basic =>
            if (eval) new FactoredDecodingSeqGen[String](model, opts) with BasicSeqGen with SeqGenScorer[String]
            else new FactoredDecodingSeqGen[String](model, opts) with BasicSeqGen
        }
      }
    decoder.setDecoder(true)
    decoder
  }

  def getPreDecoders() = opts.preModels.map { file => getDecoder(file, false) }

  private def applyDecoders(preDecoders: Seq[StdDecoder], finalDecoder: StdDecoder, f: String, ofile: Option[String]): Unit =
    applyDecoders(preDecoders, finalDecoder, new java.io.File(f), ofile map { new java.io.File(_) })

  private def applyDecoders(preDecoders: Seq[StdDecoder], finalDecoder: StdDecoder, f: java.io.File, ofile: Option[java.io.File]): Unit =
    preDecoders match {
      case first :: rest =>
        val deserial = first.sGen.deserializeFromFile(f)
        val srcs = first.decodeToSources(Set("lex"), "pre_", f)
        preDecoders.foreach { d => d.decodeSources(Set("lex"), "pre_", srcs) }
        finalDecoder.decodeToFile(srcs, f, ofile) // final "second stage" decoder
      case Nil =>
    }

  def processWithPipeline(preDecoders: Seq[StdDecoder], finalDecoder: StdDecoder): Unit = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) => new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new java.io.File(dirStr)
        if (!dir.exists) {
          System.err.println(">>> Input directory invalid <<<\n")
          throw new RuntimeException("I/o failure")
        }
        val fs =
          dir.listFiles filter
            { f: java.io.File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } }
        val osuffix = opts.outSuffix match { case Some(o) => o case None => "" }
        fs foreach { f: java.io.File =>
          val ofile = opts.outputDir map { d => new java.io.File(d + "/" + f.getName + osuffix) }
          ofile foreach { f =>
            if (!f.getParentFile.exists) {
              System.err.println(">>> Output directory/file invalid <<<\n")
              throw new RuntimeException("I/o failure")
            }
          }
          applyDecoders(preDecoders, finalDecoder, f, ofile)
          print(".")
        }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            val ifile = new java.io.File(f)
            if (!ifile.exists) { System.err.println(">>> Input file invalid <<<"); throw new RuntimeException("I/o failure") }
            val ifileDir = ifile.getParentFile
            val ofile = opts.outputFile map { new java.io.File(_) }
            ofile foreach { f => 
              val pdir = f.getParentFile
              val pd = if (pdir == null) ifileDir else pdir
              if (!pd.exists) { System.err.println(">>> Output file invalid <<<"); throw new RuntimeException("I/o failure") } }
            applyDecoders(preDecoders, finalDecoder, ifile, ofile)
          case None =>
            throw new RuntimeException("Expecting input file or input directory")
        }
    }
  }

  def getTrainer() = {
    val st =
      new StdTrainer(opts) {
        val sGen = mode match {
          case Text => new TrainingSeqGen[String](opts) with TextSeqGen
          case Json => new TrainingSeqGen[String](opts) with JsonSeqGen
          case Basic => new TrainingSeqGen[String](opts) with BasicSeqGen
        }
      }
    st
  }

  def trainWithPreDecoders(trainer: Trainer[String]) = {
    val decoders = getPreDecoders() // get pre-decoders
    val srcs =
      decoders.zipWithIndex match {
        case (first, i) :: rest =>
          val srcs = first.decodeToSources(Set("lex"), "pre_")
          rest.foreach { case (el, i) => el.decodeSources(Set("lex"), "pre_", srcs) }
          Some(srcs)
        case Nil => None
      }
    val trsrcs = trainer.sGen.createSourcesFromFiles.flatten
    srcs match {
      case Some(s) =>
        (trsrcs.toList zip s.toList) foreach { case (tseq, seq) => seq.updateLabels(tseq) }
        trainer.trainFromSeqs(s)
      case None => trainer.trainFromSeqs(trsrcs)
    }
  }

  def process() = {
    opts.checkRequired("--model")
    if (opts.train) {
      val trainer = getTrainer()
      if (opts.preModels.length > 0) {
        trainWithPreDecoders(trainer)
      } else {
        trainer.train()
      }
    } else {
      val eval = opts.evaluate match { case Some(_) => true case None => false }
      val start = System.nanoTime
      if (opts.preModels.size > 0) {
        val preDecoders = getPreDecoders()
        val finalDecoder = getDecoder(opts.model.get, eval)
        processWithPipeline(preDecoders, finalDecoder)
        if (eval) finalDecoder.sGen.getAccuracy
      } else {
        val decoder = getDecoder(opts.model.get, eval)
        decoder.decode()
        decoder.cleanUp()
        if (eval) decoder.sGen.getAccuracy
      }
      val took = System.nanoTime - start
      println("\nFinished decoding in " + (took / 1000000000.0) + " seconds.\n")
    }
  }
}

object TaggerTask {
  def printUsage =
    println("\n\n Usage: java -jar jcarafe-0.9.8.2-bin.jar <options>\n\n Issue java -jar jcarafe-0.9.8-2-bin.jar --help for a list of command line options\n\n")
  def apply(argv: Array[String]) = {
    val opts = new Options(argv)
    if (opts.selfInducedIterations > 0)
      new SemiSupervisedTaggerTask(opts)
    else
      new StdTaggerTask(opts)
  }
}
