/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf
import scala.collection.mutable.ListBuffer
import org.mitre.jcarafe.tokenizer.FastTokenizer
import org.mitre.jcarafe.util._
import scala.actors._

abstract class Decoder[Obs](dynamic: Boolean, opts: Options) {

  def this() = this(false, new Options)
  def this(o: Options) = this(false, o)
  type M <: Model
  val sGen: DecodingSeqGen[Obs]
  val model: M
  lazy val ss = model.segSize

  lazy val viterbiDecoder: DecodingAlgorithm = Viterbi(dynamic, ss, model.crf, opts.posteriors)

  class DecoderWorker(val decoder: DecodingAlgorithm) extends Actor {
    def act() {
      Actor.loop {
        react {
          case seq: SourceSequence[Obs] =>
            val iseq = sGen.extractFeatures(seq)
            decoder.assignBestSequence(iseq)
            reply(iseq)
          case _ => exit()
        }
      }
    }
  }

  val numDecWorkers: Int = opts.numThreads match {
    case Some(n) => n.toInt
    case None => 1
  }
  var decoderWorkers: Vector[Actor] = Vector()

  // this should be called by concrete class constructors
  def setDecoder(b: Boolean): Unit = {
    model.fixAlphabet(b)
    if (model.beg) sGen.setBegin
    if (numDecWorkers > 1) {
      System.setProperty("actors.corePoolSize", numDecWorkers.toString)
      decoderWorkers = Vector.tabulate(numDecWorkers) { _ => (new DecoderWorker(viterbiDecoder.getCopyOf)).start }
    }
  }

  def decodeString(s: String): String = {
    val dobj = sGen.deserializeFromString(s)
    val seqs = sGen.createSeqsWithInput(dobj)
    val viterbiInstance = Viterbi(dynamic, ss, model.crf, opts.posteriors)
    seqs foreach viterbiInstance.assignBestSequence
    sGen.seqsToString(dobj, seqs)
  }

  def decodeDeserializationToAnnotations(dobj: sGen.DeserializationT): Array[Annotation] = {
    val seqs = sGen.createSeqsWithInput(dobj)
    seqs foreach viterbiDecoder.assignBestSequence
    val atbl = sGen.seqsToAnnotations(dobj, seqs)
    val lbuf = new ListBuffer[Annotation]
    atbl foreach { case (_, v) => v foreach { lbuf += _ } }
    lbuf.toArray
  }

  def decodeStringToAnnotations(s: String): Array[Annotation] = {
    val dobj = sGen.deserializeFromString(s)
    decodeDeserializationToAnnotations(dobj)
  }

  def decodeTokenSequenceToStates(seq: Seq[String]): Seq[String] = {
    val dobj = sGen.deserializeFromTokenSeq(seq)
    val seqs = sGen.createSeqsWithInput(dobj)
    val invMap = sGen.invLa
    val sBuf = new collection.mutable.ListBuffer[String]
    seqs foreach { seq =>
      viterbiDecoder.assignBestSequence(seq);
      seq.iseq foreach { el => sBuf append invMap(el.label).toString }
    }
    sBuf.toSeq
  }

  def decodeTokenSequenceToStatesJavaList(seq: java.util.List[String]): java.util.List[String] = {
    import scala.collection.JavaConversions._
    decodeTokenSequenceToStates(seq)
  }

  def decodeToAnnotations(s: String): Array[Annotation] = {
    val dobj = sGen.deserializeFromRawString(s)
    decodeDeserializationToAnnotations(dobj)
  }

  def decodeSeqsToAnnotations(str: String, seqs: Seq[SourceSequence[Obs]]): Array[Annotation] = {
    val dobj = sGen.deserializeFromString(str)
    val iseqs = seqs map sGen.extractFeatures
    iseqs foreach viterbiDecoder.assignBestSequence
    val atbl = sGen.seqsToAnnotations(dobj, iseqs)
    val lbuf = new ListBuffer[Annotation]
    atbl foreach { case (_, v) => v foreach { lbuf += _ } }
    lbuf.toArray
  }

  def decodeSeqsToString(str: String, seqs: Seq[SourceSequence[Obs]]): String = {
    val dobj = sGen.deserializeFromString(str)
    val iseqs = seqs map sGen.extractFeatures
    iseqs foreach viterbiDecoder.assignBestSequence
    sGen.seqsToString(dobj, iseqs)
  }

  def decodeToFile(s: Seq[SourceSequence[Obs]], f: java.io.File, ofile: Option[java.io.File]) = {
    val deser = sGen.deserializeFromFile(f)
    applyDecoder(s, deser, viterbiDecoder, ofile match { case Some(ofile) => Some(ofile.toString) case None => None })
  }

  def decodeSources(exceptions: Set[String], id: String, s: Seq[SourceSequence[Obs]]) = decodeSeqsToSources(exceptions, id, s, viterbiDecoder)
  def decodeToSources(exceptions: Set[String], id: String, f: java.io.File) = decodeSeqsToSourcesFromFile(exceptions, id, f, viterbiDecoder)
  def decodeToSources(exceptions: Set[String], id: String) = decodeSeqsToSourcesFromFiles(exceptions, id, viterbiDecoder)
  def decodeToSources(exceptions: Set[String], id: String, s: String) = decodeSeqsToSourcesFromString(exceptions, id, s, viterbiDecoder)

  def decode(): Unit = decodeSeqsFromFiles(viterbiDecoder)
  def cleanUp(): Unit = decoderWorkers foreach { _ ! None }

  private def applyToSeqsInParallel(seqs: Seq[SourceSequence[Obs]], decoder: DecodingAlgorithm): Seq[InstanceSequence] = {
    val futures = for ((f, i) <- seqs.zipWithIndex) yield decoderWorkers(i % numDecWorkers) !! f
    futures map { f => f() match { case r: InstanceSequence => r case _ => throw new RuntimeException("Future failed") } }
  }

  private def applyToSeqs(seqs: Seq[SourceSequence[Obs]], decoder: DecodingAlgorithm): Seq[InstanceSequence] = {
    seqs map { s =>
      val iseq = sGen.extractFeatures(s)
      decoder.assignBestSequence(iseq)
      iseq
    }
  }

  private def applyToSeqsInParallel(seqs: Seq[SourceSequence[Obs]], dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]): Unit = {
    val iseqs = applyToSeqsInParallel(seqs, decoder)
    opts.evaluate match {
      case Some(_) =>
        sGen.evaluateSequences(iseqs)
      case None =>
        outFile match { case Some(outFile) => sGen.seqsToFile(dobj, iseqs, new java.io.File(outFile)) case None => throw new RuntimeException("Expected output directory") }
    }
  }

  def applyDecoderParallel(dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]): Unit = {
    val seqs = sGen.toSources(dobj)
    applyToSeqsInParallel(seqs, dobj, decoder, outFile)
  }

  def applyDecoder(dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]): Unit = {
    val seqs = sGen.createSeqsWithInput(dobj)
    seqs foreach { decoder.assignBestSequence(_) }
    opts.evaluate match {
      case Some(_) =>
        sGen.evaluateSequences(seqs)
      case None =>
        outFile match { case Some(outFile) => sGen.seqsToFile(dobj, seqs, new java.io.File(outFile)) case None => throw new RuntimeException("Expected output directory") }
    }
  }

  def applyDecoder(srcs: Seq[SourceSequence[Obs]], dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]): Unit = {
    opts.evaluate match {
      case Some(_) =>
        val nsrcs = sGen.toSources(dobj)
        (nsrcs.toList zip srcs.toList) foreach { case (tseq, seq) => seq.updateLabels(tseq) }
        val seqs = sGen.extractFeatures(srcs)
        seqs foreach { decoder.assignBestSequence(_) }
        sGen.evaluateSequences(seqs)
      case None =>
        val seqs = sGen.extractFeatures(srcs)
        seqs foreach { decoder.assignBestSequence(_) }
        outFile match { case Some(outFile) => sGen.seqsToFile(dobj, seqs, new java.io.File(outFile)) case None => throw new RuntimeException("Expected output directory") }
    }
  }

  private def applyDecoderParallel(srcs: Seq[SourceSequence[Obs]], dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]) = {
    val nsrcs = sGen.toSources(dobj)
    (nsrcs.toList zip srcs.toList) foreach { case (tseq, seq) => seq.updateLabels(tseq) }
    applyToSeqsInParallel(srcs, dobj, decoder, outFile)
  }

  def runDecoder(dobj: sGen.DeserializationT, decoder: DecodingAlgorithm, outFile: Option[String]): Unit = {
    if (numDecWorkers > 1)
      applyDecoderParallel(dobj, decoder, outFile)
    else
      applyDecoder(dobj, decoder, outFile)
  }

  def decodeSeqsFromFiles(decoder: DecodingAlgorithm): Unit = {
    opts.inputDir match {
      case Some(dirStr) =>
        val pat = opts.inputFilter match {
          case Some(r) =>
            new scala.util.matching.Regex(r)
          case None => new scala.util.matching.Regex(".*")
        }
        val dir = new java.io.File(dirStr)
        val odir = opts.outputDir
        val fs =
          dir.listFiles filter
            { f: java.io.File => pat.findFirstIn(f.toString) match { case Some(_) => true case None => false } }
        val osuffix = opts.outSuffix match { case Some(o) => o case None => "" }
        fs foreach { f =>
          val ofile = opts.outputDir match { case Some(d) => Some(d + "/" + f.getName + osuffix) case None => None }
          val deser = sGen.deserializeFromFile(f)
          runDecoder(deser, decoder, ofile)
          print(".")
        }
      case None =>
        opts.inputFile match {
          case Some(f) =>
            val deser = sGen.deserializeFromFile(f)
            runDecoder(deser, decoder, opts.outputFile)
          case None =>
            throw new RuntimeException("Expecting input file or input directory")
        }
    }
  }

  /*
   * This method facilitates basic pipelining of decoders. It may be more generally useful and so isn't 
   * in a separate trait (yet?)
  */
  def decodeSeqsToSources(exceptions: Set[String], id: String, seqs: Seq[SourceSequence[Obs]], decoder: DecodingAlgorithm): Unit = {
    val iseqs = if (numDecWorkers > 1) applyToSeqsInParallel(seqs, decoder) else applyToSeqs(seqs, decoder)
    for (i <- 0 until seqs.length) {
      val lSeq = iseqs(i).iseq
      val si = seqs(i)
      for (j <- 0 until lSeq.length) {
        val cc = si(j).preLabelCode
        val nc = IncrementalMurmurHash.mix(lSeq(j).label, cc)
        si(j).preLabelCode_=(nc)
      }
    }
  }

  /*
   * This creates a set of Seqs for the entire set of files specified. This can be used to apply decoders
   * before a final TRAINING phase.
  */
  def decodeSeqsToSourcesFromFiles(exceptions: Set[String], id: String, decoder: DecodingAlgorithm): sGen.Seqs = {
    val srcs = sGen.createSourcesFromFiles
    srcs foreach { src => decodeSeqsToSources(exceptions, id, src, decoder) }
    srcs.flatten
  }

  def decodeSeqsToSourcesFromDeserialization(exceptions: Set[String], id: String, d: sGen.DeserializationT, decoder: DecodingAlgorithm): sGen.Seqs = {
    val srcs = sGen.toSources(d)
    decodeSeqsToSources(exceptions, id, srcs, decoder)
    srcs
  }

  def decodeSeqsToSourcesFromDeserialization(exceptions: Set[String], id: String, d: sGen.DeserializationT): sGen.Seqs = {
    decodeSeqsToSourcesFromDeserialization(exceptions, id, d, viterbiDecoder)
  }

  /*
   * This creates a Seqs for just the single file provided. Used for run-time decoding.
  */
  def decodeSeqsToSourcesFromFile(exceptions: Set[String], id: String, f: java.io.File, decoder: DecodingAlgorithm): sGen.Seqs =
    decodeSeqsToSourcesFromDeserialization(exceptions, id, sGen.deserializeFromFile(f), decoder)

  def decodeSeqsToSourcesFromString(exceptions: Set[String], id: String, s: String, decoder: DecodingAlgorithm): sGen.Seqs =
    decodeSeqsToSourcesFromDeserialization(exceptions, id, sGen.deserializeFromString(s), decoder)

}

abstract class FactoredDecoder[Obs](o: Options) extends Decoder[Obs](o) {
  type M = StdModel
  //override def cleanUp() = sGen.cleanUp()
}

abstract class NonFactoredDecoder[Obs](dyn: Boolean, o: Options) extends Decoder[Obs](dyn, o) {
  def this() = this(false, new Options)
  def this(o: Options) = this(false, o)
  type M = NonFactoredModel
}

abstract class StdDecoder(opts: Options, m: String, r: Option[java.io.InputStream]) extends FactoredDecoder[String](opts) {
  import StandardSerializer._
  def this(opts: Option[Options], m: String) = this(opts.getOrElse(new Options), m, None)
  def this(opts: Options, m: String) = this(opts, m, None)
  def this(opts: Options, r: java.io.InputStream) = this(opts, "", Some(r))
  def this(opts: Option[Options], r: java.io.InputStream) = this(opts.getOrElse(new Options), "", Some(r))
  def this(opts: Options, r: Option[java.io.InputStream]) = this(opts, "", r)
  def this(opts: Options) = this(opts, opts.model.get)
  def this(r: java.io.InputStream) = this(None, r)
  def this(m: String, r: Option[java.io.InputStream]) = this(new Options, m, r)
  def this(m: String) = this(None, m)
  val model: M = {
    val model = r match { case Some(r) => readModel(r) case None => readModel(m) }
    opts.priorAdjust match { case Some(v) => model.adjustParameter(IncrementalMurmurHash.hash(":U:", 0), SLabel("lex"), v) case None => }
    model
  }
}

object Decoder {
  def apply(args: Array[String]): StdDecoder = {
    apply(new Options(args))
  }

  def apply(opts: Options): StdDecoder = {
    opts.checkRequired("--model", "--mode")
    new StdDecoder(opts) {
      val sGen = opts.mode match {
        case Some("inline") => new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen
        case _ => new FactoredDecodingSeqGen[String](model, opts) with JsonSeqGen
      }
      setDecoder(true)
    }
  }
}

class JsonDecoder(opts: Options, m: String, r: Option[java.io.InputStream]) extends StdDecoder(opts, m, r) {
  def this(r: java.io.InputStream) = this(new Options, "", Some(r))
  def this(s: String) = this(new Options, s, None)
  def this(opts: Options) = this(opts, opts.model.get, None)
  def this(opts: Options, r: java.io.InputStream) = this(opts, "", Some(r))
  def this(opts: Options, m: String) = this(opts, m, None)
  def this(args: Array[String], m: String) = this(new Options(args), m)
  val sGen = new FactoredDecodingSeqGen[String](model, opts) with JsonSeqGen
  setDecoder(true)
}

class TextDecoder(opts: Options, m: String, r: Option[java.io.InputStream]) extends StdDecoder(opts, m, r) {
  def this(r: java.io.InputStream) = this(new Options, "", Some(r))
  def this(s: String) = this(new Options, s, None)
  def this(opts: Options) = this(opts, opts.model.get, None)
  def this(opts: Options, r: java.io.InputStream) = this(opts, "", Some(r))
  def this(opts: Options, m: String) = this(opts, m, None)
  def this(args: Array[String], m: String) = this(new Options(args), m)

  val sGen =
    if (opts.rawDecode) {
      new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen with StreamingDecoder {
        override def deserializeFromFile(f: String) = {
          new TextSeqDeserialization(FastTokenizer.parseFileNoTags(f))
        }
      }
    } else if (opts.streaming) new FactoredDecodingSeqGen[String](model) with TextSeqGen with StreamingDecoder
    else new FactoredDecodingSeqGen[String](model, opts) with TextSeqGen
  setDecoder(true)
}

class BasicDecoder(opts: Options, m: String, r: Option[java.io.InputStream]) extends StdDecoder(opts, m, r) {
  def this(r: java.io.InputStream) = this(new Options, "", Some(r))
  def this(s: String) = this(new Options, s, None)
  def this(opts: Options) = this(opts, opts.model.get, None)
  def this(opts: Options, r: java.io.InputStream) = this(opts, "", Some(r))
  def this(opts: Options, m: String) = this(opts, m, None)
  def this(args: Array[String], m: String) = this(new Options(args), m)

  val sGen = new FactoredDecodingSeqGen[String](model, opts) with BasicSeqGen
  setDecoder(true)
}

