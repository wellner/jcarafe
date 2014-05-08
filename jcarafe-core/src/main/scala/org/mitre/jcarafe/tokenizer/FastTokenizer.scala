/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.tokenizer

import org.mitre.jcarafe.lexer.{
  Token,
  GenTokerConstants,
  SimpleCharStream,
  GenToker,
  JsonToker,
  WhiteSpaceToker,
  JsonTokerConstants,
  WhiteSpaceTokerConstants
}
import org.mitre.jcarafe.util._
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Reader
import GenTokerConstants._
import collection.mutable.ListBuffer

class TokenizerOptions extends CommandLineHandler {
  "--json" flag "Use Json Input and output"
  "--whitespace" flag "White-space tokenize"
  "--handle-tags" flag "Recognize/tokenize XML tag elements (in JSON mode)"
  "--input-file" desc "Input file"
  "--output-file" desc "Outputfile"
  "--input-dir" desc "Input directory"
  "--output-dir" desc "Output directory"
  "--region" multi "Region to tokenize"
  "--regionset" desc "File containing region specifications"
  "--tokenizer-patterns" desc "File with split-merge tokenization post-processing patterns"
}

object CharStr {
  var isSet = false
  var scs: Option[SimpleCharStream] = None
}

object FastTokenizer {

  import CharStr._
  import org.mitre.jcarafe.util.JsonAnnotationHandler._
  import Tagset.parseTagSpec
  import Tagset.loadTagset

  var isSetJson = false

  var splittingAugmenter: Option[SplitTokenizerAugmenterPattern] = None
  var mergingAugmenter: Option[MergeTokenizerAugmenterPattern] = None

  private def softEndPunct(s: String) = {
    (s equals ".") || (s equals "?") || (s equals "!") || (s equals "''")
  }

  private def parseLoop(parser: GenToker, tbuf: ListBuffer[Element], keepLexTags: Boolean = false) = {
    var c = true
    var withinPreExistingLex = false
    val preExistingBuilder = new StringBuilder
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case ENDPUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append SoftEndTok(t.image)
        case TAGSTART =>
          val img = t.image
          if (img.startsWith("<lex")) {
            preExistingBuilder.clear()
            withinPreExistingLex = true
            if (keepLexTags) tbuf append Tag(t.image, true)
          } else
            tbuf append Tag(t.image, true)
        case TAGEND =>
          val img = t.image
          if (img.equals("</lex>")) {
            val bb = preExistingBuilder.toString()
            if (softEndPunct(bb)) tbuf append SoftEndTok(bb)
            else tbuf append Tok(bb)
            withinPreExistingLex = false
            if (keepLexTags) tbuf append Tag(t.image, false)
          } else tbuf append Tag(t.image, false)
        case PUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case TOK =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case ABBREV =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case WHITE =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Ws(t.image)
        case WHITEEND =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append EndWs(t.image)
        case _ =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
      }
    }
    if (withinPreExistingLex) throw new RuntimeException("Lexer failed: open interpreted <lex..> tag without corresponding close tag")
  }

  private def parseLoopSplitting(parser: GenToker, tbuf: ListBuffer[Element], splitter: SplitTokenizerAugmenterPattern, keepLexTags: Boolean = false) = {
    var c = true
    var withinPreExistingLex = false
    val preExistingBuilder = new StringBuilder
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case ENDPUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append SoftEndTok(t.image)
        case TAGSTART =>
          val img = t.image
          if (img.startsWith("<lex")) {
            preExistingBuilder.clear()
            withinPreExistingLex = true
            if (keepLexTags) tbuf append Tag(t.image, true)
          } else
            tbuf append Tag(t.image, true)
        case TAGEND =>
          val img = t.image
          if (img.equals("</lex>")) {
            val bb = preExistingBuilder.toString()
            if (softEndPunct(bb)) tbuf append SoftEndTok(bb)
            else tbuf append Tok(bb)
            withinPreExistingLex = false
            if (keepLexTags) tbuf append Tag(t.image, false)
          } else tbuf append Tag(t.image, false)
        case PUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case TOK =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else splitter.split(t.image, t.kind) foreach { tbuf append _ }
        case ABBREV =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case WHITE =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Ws(t.image)
        case WHITEEND =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append EndWs(t.image)
        case _ =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
      }
    }
    if (withinPreExistingLex) throw new RuntimeException("Lexer failed: open interpreted <lex..> tag without corresponding close tag")
  }

  private def parse(scs: Reader, keep: Boolean = false) = {
    //if (isSet) GenTokerTokenManager.ReInit(scs) else {val _ = new GenTokerTokenManager(scs); ()}
    isSet = true
    val parser = new GenToker(scs)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    splittingAugmenter match {
      case Some(splitter) => parseLoopSplitting(parser, tbuf, splitter, keep)
      case None => parseLoop(parser, tbuf, keep)
    }
    applyMergePatterns(tbuf)
  }

  private def parseLoopNoTags(parser: WhiteSpaceToker, tbuf: ListBuffer[Element]) = {
    var c = true
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case WhiteSpaceTokerConstants.EOF => c = false
        case WhiteSpaceTokerConstants.TOK => tbuf append Tok(t.image)
        case WhiteSpaceTokerConstants.WHITE | WhiteSpaceTokerConstants.WHITEEND => tbuf append Ws(t.image)
        case _ => tbuf append Tok(t.image)
      }
    }
  }

  private def parseLoopNoTags(parser: JsonToker, tbuf: ListBuffer[Element]) = {
    var c = true
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case JsonTokerConstants.EOF => c = false
        case JsonTokerConstants.ENDPUNCT => tbuf append SoftEndTok(t.image)
        case JsonTokerConstants.PUNCT => tbuf append Tok(t.image)
        case JsonTokerConstants.TOK => tbuf append Tok(t.image)
        case JsonTokerConstants.ABBREV => tbuf append Tok(t.image)
        case JsonTokerConstants.WHITE => tbuf append Ws(t.image)
        case JsonTokerConstants.WHITEEND => tbuf append EndWs(t.image)
        case _ => tbuf append Tok(t.image)
      }
    }
  }

  private def parseLoopNoTagsSplitting(parser: JsonToker, tbuf: ListBuffer[Element], splitter: SplitTokenizerAugmenterPattern) = {
    var c = true
    var withinPreExistingLex = false
    val preExistingBuilder = new StringBuilder
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case JsonTokerConstants.EOF => c = false
        case JsonTokerConstants.ENDPUNCT => tbuf append SoftEndTok(t.image)
        case JsonTokerConstants.PUNCT => tbuf append Tok(t.image)
        case JsonTokerConstants.TOK => splitter.split(t.image, t.kind) foreach { tbuf append _ }
        case JsonTokerConstants.ABBREV => tbuf append Tok(t.image)
        case JsonTokerConstants.WHITE => tbuf append Ws(t.image)
        case JsonTokerConstants.WHITEEND => tbuf append EndWs(t.image)
        case _ => tbuf append Tok(t.image)
      }
    }
  }

  private def parseNoTags(scs: Reader) = {
    //if (isSet) JsonTokerTokenManager.ReInit(scs) else {val _ = new JsonTokerTokenManager(scs); ()}
    val parser = new JsonToker(scs)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    splittingAugmenter match {
      case Some(splitter) => parseLoopNoTagsSplitting(parser, tbuf, splitter)
      case None => parseLoopNoTags(parser, tbuf)
    }
    applyMergePatterns(tbuf)
  }

  private def applyMergePatterns(tbuf: ListBuffer[Element]) = {
    mergingAugmenter match {
      case Some(merger) =>
        val nbuf = new collection.mutable.ListBuffer[Element]
        merger.merge(tbuf.toList, nbuf)
        nbuf.toList
      case None => tbuf.toList
    }
  }

  private def printTok(at: Boolean, s: String, os: java.io.OutputStreamWriter) = {
    if (at) os.write("<lex>")
    os.write(s)
    if (at) os.write("</lex>")
  }

  def jsonTokenizeString(istr: String, parseTags: Boolean, zoneTags: Option[Tagset], whiteTok: Boolean): String = {
    val json = Json.constructJsonTypeOfString(istr)
    val njson = jsonTokenize(json, parseTags, zoneTags, whiteTok)
    Json.writeJsonToString(njson)
  }

  def jsonTokenizeString(istr: String, whiteTok: Boolean): String = {
    jsonTokenizeString(istr, false, None, whiteTok)
  }

  private def jsonTokenize(json: JsonType, parseTags: Boolean, zoneTags: Option[Tagset], whiteTok: Boolean): JsonType = {
    val orig_signal = json match {
      case JsObject(o) =>
        o("signal") match { case JsString(s) => s case _ => throw new RuntimeException("Expected signal to be a string") }
      case _ => throw new RuntimeException("No signal found")
    }
    val toks = new scala.collection.mutable.ListBuffer[JsArray]
    val sentences = new scala.collection.mutable.ListBuffer[JsArray]
    val zoneSet: Tagset = zoneTags match { case Some(z) => z case None => new Tagset(Set(Label("zone", Map("region_type" -> "body")))) }
    val asets = json match { case JsObject(o) => try { o("asets") } catch { case _: Throwable => new JsArray(Nil) } case _ => JsArray(Nil) }
    val zones: List[Annotation] = getAnnotations(Some(orig_signal), asets, zoneSet) match {
      case Nil => List(new Annotation(0, orig_signal.length, false, SLabel("zone"), None))
      case a => a
    }
    val signals: List[(String, Annotation)] = zones map { an => (orig_signal.substring(an.st, an.en), an) }
    signals foreach {
      case (signal, an) =>
        var pos = an.st
        var sentPos = an.st
        var nonWhiteTokExists = false
        val tbuf = new scala.collection.mutable.ListBuffer[Element]
        if (parseTags) {
          val parser = new GenToker(new ByteArrayInputStream(signal.getBytes))
          splittingAugmenter match {
            case Some(splitter) => parseLoopSplitting(parser, tbuf, splitter)
            case None => parseLoop(parser, tbuf)
          }
        } else if (whiteTok) {
          val parser = new WhiteSpaceToker(new ByteArrayInputStream(signal.getBytes))
          parseLoopNoTags(parser, tbuf)
        } else {
          val parser = new JsonToker(new ByteArrayInputStream(signal.getBytes))
          splittingAugmenter match {
            case Some(splitter) => parseLoopNoTagsSplitting(parser, tbuf, splitter)
            case None => parseLoopNoTags(parser, tbuf)
          }
        }
        val tokSeq = applyMergePatterns(tbuf)
        tokSeq foreach { tk =>
          var ll = 0
          tk match {
            case Ws(t) => ll = t.length
            case EndWs(_) | SoftEndTok(_) => // in this case, we start a new sentence
              ll = tk.getString.length
              val curEnd = pos + ll
              if (nonWhiteTokExists) {                
                sentences += JsArray(List(JsInt(sentPos), JsInt(curEnd)))
              }
              sentPos = curEnd         
              nonWhiteTokExists = false
            case t =>
              ll = t.getString.length
              nonWhiteTokExists = true
              toks += JsArray(List(JsInt(pos), JsInt(pos + ll)))
          }                    
          pos += ll
        }
        if (pos > sentPos) {
          sentences += JsArray(List(JsInt(sentPos), JsInt(pos)))
        }
    }
    val newToks: JsonType = JsObject(Map("type" -> JsString("lex"), "attrs" -> JsArray(Nil),
      "annots" -> (JsArray(toks.toList))))
    val newSents: JsonType = JsObject(Map("type" -> JsString("sentence"), "attrs" -> JsArray(Nil),
        "annots" -> (JsArray(sentences.toList))))
    val newJsonObj = json match {
      case JsObject(obj) =>
        val a1 =
          (try { obj("asets") match { case JsArray(a) => a case _ => throw new RuntimeException("Invalid obj") } }
          catch { case e: java.util.NoSuchElementException => Nil case e: Throwable => throw e })
        JsObject(obj.updated("asets", JsArray((newSents :: newToks :: a1).reverse)))
      case a => a
    }
    newJsonObj
  }

  def jsonTokenize(ifile: String, ofile: String, parseTags: Boolean, zoneTags: Option[Tagset], whiteTok: Boolean): Unit = {
    val json = Json.constructJsonType(ifile)
    val njson: JsonType = jsonTokenize(json, parseTags, zoneTags, whiteTok)
    Json.writeJson(njson, ofile)
  }

  private def rawTokenize(ifile: String, os: java.io.OutputStreamWriter, whiteOnly: Boolean = false) = {
    val sr = new java.io.FileInputStream(ifile)
    val reader = new InputStreamReader(sr, "UTF-8")
    val parser = new GenToker(reader)
    val whiteParser = new WhiteSpaceToker(reader)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    var c = true
    var ntoks = 0
    var withinPreExistingLex = false
    val preExistingBuilder = new StringBuilder
    while (c) {
      val t: Token = if (whiteOnly) whiteParser.getNextToken else parser.getNextToken()
      t.kind match {
        case EOF | WhiteSpaceTokerConstants.EOF => c = false
        case ENDPUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { ntoks += 1; printTok(true, t.image, os) }
        case TAGSTART =>
          val img = t.image
          if (img.startsWith("<lex")) {
            preExistingBuilder.clear()
            withinPreExistingLex = true
          } else printTok(false, t.image, os)
        case TAGEND =>
          val img = t.image
          if (img.equals("</lex>")) {
            val bb = preExistingBuilder.toString()
            printTok(true, bb, os)
            withinPreExistingLex = false
          } else printTok(false, t.image, os)
        case PUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else {
            ntoks += 1
            printTok(true, t.image, os)
          }
        case TOK | WhiteSpaceTokerConstants.TOK =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { ntoks += 1; printTok(true, t.image, os) }
        case ABBREV =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { ntoks += 1; printTok(true, t.image, os) }
        case WHITE | WhiteSpaceTokerConstants.WHITE =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { printTok(false, t.image, os) }
        case WHITEEND | WhiteSpaceTokerConstants.WHITEEND =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { printTok(false, t.image, os) }
        case URL =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { ntoks += 1; printTok(true, t.image, os) }
        case _ =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else { printTok(false, t.image, os) }
      }
    }
    if (withinPreExistingLex) throw new RuntimeException("Lexer failed: open interpreted <lex..> tag without corresponding close tag")
    ntoks
  }

  private def rawTokenizeWithSplitMerge(ifile: String, os: java.io.OutputStreamWriter) = {
    val sr = new java.io.FileInputStream(ifile)
    val reader = new InputStreamReader(sr, "UTF-8")
    val parser = new GenToker(reader)
    val tbuf = new scala.collection.mutable.ListBuffer[Element]
    var c = true
    var ntoks = 0
    var withinPreExistingLex = false
    val preExistingBuilder = new StringBuilder
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case ENDPUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append SoftEndTok(t.image)
        case TAGSTART =>
          val img = t.image
          if (img.startsWith("<lex")) {
            preExistingBuilder.clear()
            withinPreExistingLex = true
          } else tbuf append Tag(t.image, true)
        case TAGEND =>
          val img = t.image
          if (img.equals("</lex>")) {
            val bb = preExistingBuilder.toString()
            tbuf append Tok(bb)
            withinPreExistingLex = false
          } else tbuf append Tag(t.image, false)
        case PUNCT =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case TOK =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else {
            if (splittingAugmenter.isDefined) {
              splittingAugmenter.get.split(t.image, t.kind) foreach { tbuf append _ }
            } else tbuf append Tok(t.image)
          }
        case ABBREV =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Tok(t.image)
        case WHITE =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append Ws(t.image)
        case WHITEEND =>
          if (withinPreExistingLex) preExistingBuilder append t.image
          else tbuf append EndWs(t.image)
        case EOF => c = false
        case _ => if (withinPreExistingLex) preExistingBuilder append t.image else tbuf append Tok(t.image)
      }
    }
    if (withinPreExistingLex) throw new RuntimeException("Lexer failed: open interpreted <lex..> tag without corresponding close tag")
    mergingAugmenter match {
      case Some(merger) =>
        val nbuf = new collection.mutable.ListBuffer[Element]
        merger.merge(tbuf.toList, nbuf)
        nbuf foreach {
          case Tok(t) => printTok(true, t, os)
          case SoftEndTok(t) => printTok(true, t, os)
          case a => printTok(false, a.getString, os)
        }
      case None =>
        tbuf foreach {
          case Tok(t) => printTok(true, t, os)
          case SoftEndTok(t) => printTok(true, t, os)
          case a => printTok(false, a.getString, os)
        }
    }
  }

  def parseString(s: String, keep: Boolean = false) = {
    val reader = new InputStreamReader(new ByteArrayInputStream(s.getBytes), "UTF-8")
    parse(reader, keep)
  }

  def parseStringNoTags(s: String) = {
    val r = parseNoTags(new InputStreamReader(new ByteArrayInputStream(s.getBytes), "UTF-8"))
    r
  }

  def parseFile(f: String, keep: Boolean = false) = {
    val fstream = new java.io.FileInputStream(f)
    val reader = new InputStreamReader(fstream, "UTF-8")
    val r = parse(reader, keep)
    fstream.close
    r
  }

  def parseFileNoTags(f: String) = {
    val fstream = new java.io.FileInputStream(f)
    val r = parseNoTags(new InputStreamReader(fstream, "UTF-8"))
    fstream.close
    r
  }

  def processFile(jsonP: Boolean, ifile: String, ofile: String, parseTags: Boolean = false, zoneSet: Option[Tagset], whiteOnly: Boolean = false) =
    if (jsonP) {
      jsonTokenize(ifile, ofile, parseTags, zoneSet, whiteOnly)
    } else {
      val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(ofile)), "UTF-8")
      if (splittingAugmenter.isDefined || mergingAugmenter.isDefined) {
        if (whiteOnly)
          System.err.println("WARNING: white space tokenization ignored with split-merge token augmentation. Using standard base tokenization.")
        rawTokenizeWithSplitMerge(ifile, os)
      } else rawTokenize(ifile, os, whiteOnly)
      os.close;
    }

  def setTokenizerAugmenters(f: java.io.File) = {
    val (cat, patterns) = (new TokenizerPatternParser).parseFile(f)
    splittingAugmenter = patterns match { case Nil => None case a => Some(SplitTokenizerAugmenterPattern(a)) }
    mergingAugmenter = patterns match { case Nil => None case a => Some(MergeTokenizerAugmenterPattern(a)) }
  }

  def main(args: Array[String]) = {
    val opts = new TokenizerOptions
    opts.process(args.toList)
    val start = System.nanoTime
    val jsonP = opts.check("--json")
    val handleTags = opts.check("--handle-tags")
    val whiteOnly = opts.check("--whitespace")
    val zoneset: Tagset =
      opts.get("--regionset") match {
        case Some(t) => loadTagset(t)
        case None =>
          val regions = opts.getAll("--region")
          new Tagset(regions.foldLeft(Set.empty: Set[AbstractLabel]) { (ac, s) => ac + parseTagSpec(s) })
      }
    opts.get("--tokenizer-patterns") match {
      case Some(tfile) =>
        setTokenizerAugmenters(new java.io.File(tfile))
      case None =>
    }
    opts.get("--input-file") match {
      case Some(ifile) =>
        val ofile = opts.get("--output-file") match {
          case Some(ofile) => processFile(jsonP, ifile, ofile, handleTags, Some(zoneset), whiteOnly)
          case None => println("Output file expected"); sys.exit(2)
        }
      case None =>
        opts.get("--input-dir") match {
          case Some(idir) =>
            val dir = new java.io.File(idir)
            opts.get("--output-dir") match {
              case Some(odir) =>
                dir.listFiles foreach { f: java.io.File =>
                  if (f.isFile) {
                    println("Processing file: " + f)
                    val ofile = odir + "/" + f.getName
                    val ifile = idir + "/" + f.getName
                    processFile(jsonP, ifile, ofile, handleTags, Some(zoneset), whiteOnly)
                  }
                }
              case None => println("Output directory expected"); sys.exit(2)
            }
          case None => println("\nMust provide either an input file and output file pair or input directory and output directory pair"); sys.exit(2)
        }
    }
    println("Tokenization completed in " + ((System.nanoTime - start) / 1000000000.0) + " seconds")
  }
}
