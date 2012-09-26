package org.mitre.jcarafe.util

import org.mitre.jcarafe.lexer._
import org.mitre.jcarafe.lexer.GenTokerConstants._
import org.mitre.jcarafe.tokenizer._

class TFIDFOptions extends CommandLineHandler {
  "--output-file" desc "Outputfile"
  "--input-dir" desc "Input directory"
  "--idf" flag "Basic IDF instead of RIDF"
}

class IdfStats(val idf: Double, val ridf: Double)

object ComputeTFIDFTable {

  val idfTable = new collection.mutable.HashMap[String, IdfStats]

  val tfTable = new collection.mutable.HashMap[String, Int]
  val dfTable = new collection.mutable.HashMap[String, Int]
  var mxVal = 0.0
  var mnVal = 0.0

  val l2 = math.log(2.0)

  def log2(x: Double) = math.log(x) / l2

  def updateTable(s: String, tbl: collection.mutable.HashMap[String, Int]) = {
    val c = tbl.get(s) match { case Some(v) => v case None => 0 }
    tbl.update(s, (c + 1))
  }

  def processFile(ifile: String) = {
    val toks = FastTokenizer.parseFile(ifile)
    val ds = new collection.mutable.HashSet[String]
    toks foreach { case Tok(t) => ds += t; updateTable(t, tfTable) case _ => }
    ds foreach { s => updateTable(s, dfTable) }
  }

  def writeTableToFile(ofile: java.io.File, numDocs: Int, idf: Boolean) = {
    val N = numDocs.toDouble
    // theta - word freq / # docs
    dfTable foreach {
      case (k, v) =>
        val idf_v = log2(N / v)
        val ex_idf = -log2(1 - math.exp(-(tfTable(k).toDouble / N)))
        val diff = idf_v - ex_idf
        if (idf) {
          if (idf_v < mnVal) mnVal = idf_v
          if (idf_v > mxVal) mxVal = idf_v
        } else {
          if (diff < mnVal) mnVal = diff
          if (diff > mxVal) mxVal = diff
        }
        idfTable.update(k, (new IdfStats(idf_v, (idf_v - ex_idf))))
    }
    val ostr = new java.io.FileOutputStream(ofile)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val tdiff = mxVal - mnVal
    idfTable.foreach {
      case (k, v) =>
        val pvl = if (idf) v.idf else v.ridf
        val vl = (pvl - mnVal) / tdiff // normalize
        if (vl > 0.001) {
          os.write(k)
          os.write('\t')
          os.write(vl.toString)
          os.write('\n')
        }
    }
    os.close()
  }

  def main(args: Array[String]) = {
    val opts = new TFIDFOptions
    opts.process(args.toList)
    val ofile =
      opts.get("--output-file") match {
        case Some(f) => new java.io.File(f)
        case None => throw new RuntimeException("Expected output file")
      }
    var numDocs = 0
    opts.get("--input-dir") match {
      case Some(idir) =>
        val dir = new java.io.File(idir)
        dir.listFiles foreach { f: java.io.File =>
          numDocs += 1
          val ifile = idir + "/" + f.getName
          processFile(ifile)
        }
      case None =>
    }
    writeTableToFile(ofile, numDocs, opts.check("--idf"))
  }

}
