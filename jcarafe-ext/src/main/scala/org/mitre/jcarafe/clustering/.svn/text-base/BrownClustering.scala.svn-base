/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.clustering
import org.mitre.jcarafe.crf.Alphabet
import org.mitre.jcarafe.util.CommandLineHandler
import org.mitre.jcarafe.lexer._
import collection.mutable.{ ArrayBuffer, Stack, ListBuffer, HashMap }

class ClusterData(val freqs: Array[Int],
  val freqOrdered: Array[Int],
  val rCntxt: Array[HistoGram],
  val lCntxt: Array[HistoGram])
  
  
class HistoGram(val hist: HashMap[Int,Int]) {
  def this() = this(new HashMap[Int,Int]())
  
  def update(k: Int) = {
    val cnt = if (hist.contains(k)) hist(k) else 0
    hist.update(k,(cnt+1))
  }
  
  def get(v: Int) = hist.get(v)
}  

final class BrownClustering(val initC: Int, val txtInput: Boolean = false, val debug: Int = 4, val verbose: Boolean = false) {

  var clusterData: Option[ClusterData] = None

  val symbolTable = new Alphabet[String]

  val revTable = new collection.mutable.HashMap[Int, String]

  var secondStageId = 0
  val ncells = initC + 2 // two extra cells for temporary swaps
  var free1 = -1 // index for extra cell 1
  var free2 = -1 // index for extra cell 2

  var curClusterIdx = 0

  val cell2cluster = Array.fill(ncells)(0) // map cells to clusters
  var cluster2cell = collection.immutable.IntMap[Int]() // map clusters/phrases to cells

  var rep2cluster = collection.immutable.IntMap[Int]() // map a phrase to the cluster containing a
  var cluster2rep = collection.immutable.IntMap[Int]() // map a cluster to the representative phrase of cluster

  val phrase2rep = UnionSet()

  var clusterTree = collection.immutable.IntMap[(Int, Int)]() // represent cluster tree

  val p1 = Array.fill(ncells)(0.0) // indexes based on cells
  val p2 = Array.fill(ncells, ncells)(0.0)
  val q2 = Array.fill(ncells, ncells)(0.0)
  val l2 = Array.fill(ncells, ncells)(0.0)

  lazy val l2Par = l2.zipWithIndex.par // parallel collection of matrix rows

  var textSize = 0
  var cur_mi = 0.0 // current mutual information value

  val logOfTwo = math.log(2.0)

  private def log2(x: Double) = math.log(x) / logOfTwo

  private def biP2(s: Int, t: Int) = if (s == t) p2(s)(s) else p2(s)(t) + p2(t)(s)

  private def biQ2(s: Int, t: Int) = if (s == t) q2(s)(s) else q2(s)(t) + q2(t)(s)

  private def hypP1(s: Int, t: Int) = p1(s) + p1(t)

  //private def hypP2(ip: (Int, Int), u: Int): Double = p2(ip._1)(u) + p2(ip._2)(u)
  //private def hypP2(u: Int, ip: (Int, Int)): Double = p2(u)(ip._1) + p2(u)(ip._2)
  private def hypP2_1(ip1: Int, ip2: Int, u: Int): Double = p2(ip1)(u) + p2(ip2)(u)
  private def hypP2_2(u: Int, ip1: Int, ip2: Int): Double = p2(u)(ip1) + p2(u)(ip2)

  /*
  private def hypP2(ip: (Int, Int)): Double = {
    val first = ip._1
    val second = ip._2
    p2(first)(first) + p2(first)(second) + p2(second)(first) + p2(second)(second)
  }
  */
  private def hypP2(ip1: Int, ip2: Int): Double = p2(ip1)(ip1) + p2(ip1)(ip2) + p2(ip2)(ip1) + p2(ip2)(ip2)

  //private def biHypP2(ip: (Int, Int), u: Int) = hypP2(ip, u) + hypP2(u, ip)
  private def biHypP2(ip1: Int, ip2: Int, u: Int) = hypP2_1(ip1, ip2, u) + hypP2_2(u, ip1, ip2)

  private def p2q(pst: Double, ps: Double, pt: Double) = if (pst == 0.0) 0.0 else pst * log2(pst / (ps * pt))

  //private def hypQ2(ip: (Int, Int), u: Int): Double = p2q(hypP2(ip, u), hypP1(ip._1, ip._2), p1(u))
  //private def hypQ2(u: Int, ip: (Int, Int)): Double = p2q(hypP2(u, ip), hypP1(ip._1, ip._2), p1(u))
  private def hypQ2_1(ip1: Int, ip2: Int, u: Int): Double = p2q(hypP2_1(ip1, ip2, u), hypP1(ip1, ip2), p1(u))
  private def hypQ2_2(u: Int, ip1: Int, ip2: Int): Double = p2q(hypP2_2(u, ip1, ip2), hypP1(ip1, ip2), p1(u))

  //private def biHypQ2(ip: (Int, Int), u: Int) = hypQ2(ip, u) + hypQ2(u, ip)
  private def biHypQ2(ip1: Int, ip2: Int, u: Int) = hypQ2_1(ip1, ip2, u) + hypQ2_2(u, ip1, ip2)

  /*
  private def hypQ2(ip: (Int, Int)) = {
    val lp = hypP2(ip)
    val bp = hypP1(ip._1, ip._2)
    p2q(lp, bp, bp)
  }
  */
  private def hypQ2(ip1: Int, ip2: Int) = {
    val lp = hypP2(ip1, ip2)
    val bp = hypP1(ip1, ip2)
    p2q(lp, bp, bp)
  }

  private def addToCell(i: Int, j: Int) = {
    cluster2cell += (i -> j)
    cell2cluster(j) = i
  }

  private def initCell(s: Int) = {
    var i = 0; while (i < ncells) {
      if (!(cell2cluster(i) < 0)) {
        p2(s)(i) = 0
        q2(s)(i) = 0
        p2(i)(s) = 0
        q2(i)(s) = 0
      }
      i += 1
    }
  }

  private def validClusterPair(s: Int, t: Int) = (!(s < 0 || t < 0)) && (cell2cluster(s) < cell2cluster(t))

  private def setP2Q2(s: Int, t: Int, c: Int) = {
    val pst = c.toDouble / textSize
    val ps = p1(s)
    val pt = p1(t)
    val qst = p2q(pst, ps, pt)
    p2(s)(t) = pst
    q2(s)(t) = qst
    qst
  }

  private def freeCells(s: Int, t: Int) = {
    free1 = s
    free2 = t
    cluster2cell -= cell2cluster(s)
    cluster2cell -= cell2cluster(t)
    cell2cluster(s) = -1
    cell2cluster(t) = -1
  }

  private def addClusterToFreeCell(a: Int) = {
    var s = -1
    if (free1 != -1) { s = free1; free1 = -1 }
    else if (free2 != -1) { s = free2; free2 = -1 }
    assert(s != -1)
    addToCell(a, s)
    s
  }

  private def createInitialClusters() = {
    var rightPhraseFreqs: collection.immutable.IntMap[Int] = collection.immutable.IntMap()
    val freqOrdered = clusterData.get.freqOrdered
    val freqs = clusterData.get.freqs
    val rCntxt = clusterData.get.rCntxt
    val lCntxt = clusterData.get.lCntxt

    freeCells(initC, initC + 1)

    phrase2rep.init(symbolTable.size)

    for (i <- 0 until initC) {
      val a = freqOrdered(i)
      addToCell(a, i)
      rep2cluster += (a -> a)
      cluster2rep += (a -> a)
    }
    for (i <- 0 until ncells) {
      val a = cell2cluster(i)
      if (a >= 0) {
        initCell(i) // initialize cell
        p1(i) = freqs(a).toDouble / textSize
      }
    }

    for (i <- 0 until ncells) {
      if (cell2cluster(i) != -1) {
        val a = cell2cluster(i)
        rightPhraseFreqs = collection.immutable.IntMap()
        if (a >= 0) {
          rCntxt(a).hist foreach { case (phr,cnt) =>
            if (cluster2cell.contains(phr)) {
              val v = rightPhraseFreqs.get(phr).getOrElse(0)
              rightPhraseFreqs += (phr -> (v + cnt))
            }
          }
          rightPhraseFreqs foreach {
            case (b, cnt) =>
              val t = cluster2cell(b)
              val d = setP2Q2(i, t, cnt)
              //println("setP2Q2(" + i + "," + t + "," + cnt + ") => " + d)			  
              cur_mi += d
          }
        }
      }
    }
  }

  private def updateTables(f: java.io.File, freqs: ArrayBuffer[Int], lCntxt: ArrayBuffer[HistoGram], rCntxt: ArrayBuffer[HistoGram]) = {
    val t = System.nanoTime()
    val ss = symbolTable.size
    val fileTxt = readDataFile(f)
    updateFreqs(freqs, fileTxt)
    updateContextPhrases(lCntxt, fileTxt, -1)
    updateContextPhrases(rCntxt, fileTxt, 1)
    if (verbose) println("..read in file: " + f + " ( in " + ((System.nanoTime - t) / 1E9) +
      " seconds, symbol table increased by " + (symbolTable.size - ss) + " to " + symbolTable.size)
  }

  private def getClusterData2(d: java.io.File, dir: Boolean): Unit = {
    val freqs = new ArrayBuffer[Int]()
    val lCntxt = new ArrayBuffer[HistoGram]()
    val rCntxt = new ArrayBuffer[HistoGram]()
    if (dir) d.listFiles foreach { d => updateTables(d, freqs, lCntxt, rCntxt) } else updateTables(d, freqs, lCntxt, rCntxt)
    println("\n... Finished reading in and setting up frequency tables ...")
    val frArray = freqs.toArray
    val orderedFreqs = getFreqOrderedTerms(frArray)
    val lctxt = lCntxt.toArray
    val rctxt = rCntxt.toArray
    clusterData = Some(new ClusterData(frArray, orderedFreqs, rctxt, lctxt))
    symbolTable foreach { case (k, v) => revTable += (v -> k) } // build reverse mapping
  }

  private def readDataFile(f: java.io.File): Array[Array[Int]] =
    if (txtInput) readTextDataFile(f)
    else readSgmlDataFile(f)

  private def readSgmlDataFile(f: java.io.File): Array[Array[Int]] = {
    import GenTokerConstants._
    val tbs = new ArrayBuffer[Array[Int]]()
    var tb: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val sr = new java.io.FileInputStream(f)
    val reader = new java.io.InputStreamReader(sr, "UTF-8")
    val parser = new GenToker(reader)
    var c = true
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case TAGSTART | TAGEND | WHITE =>
        case WHITEEND =>
          val ar = tb.toArray
          if (ar.length > 1) tbs append ar
          tb = new ArrayBuffer[Int]
        case _ =>
          val in = symbolTable.update(t.image)
          textSize += 1
          tb append in
      }
    }
    sr.close
    tbs.toArray
  }

  private def readTextDataFile(f: java.io.File): Array[Array[Int]] = {
    import WhiteSpaceToker2Constants._
    val tbs = new ArrayBuffer[Array[Int]]()
    var tb: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val sr = new java.io.FileInputStream(f)
    val parser = new WhiteSpaceToker2(sr)
    var c = true
    while (c) {
      val t: Token = parser.getNextToken()
      t.kind match {
        case EOF => c = false
        case WHITE | WHITEEND =>
        case WHITEENDLONG =>
          val ar = tb.toArray
          if (ar.length > 1) tbs append ar
          tb = new ArrayBuffer[Int]()
        case _ =>
          val in = symbolTable.update(t.image)
          textSize += 1
          tb append in
      }
    }
    val last = tb.toArray
    if (last.length > 1) tbs append last
    sr.close
    tbs.toArray
  }

  private def readData(z: java.io.File): Array[Array[Int]] = {
    var cnt = 0
    val tb = new ListBuffer[Array[Int]]
    z.listFiles foreach { f: java.io.File => readDataFile(f) foreach { tb append _ } }
    val seqs = tb.toArray
    println("\n...Read in " + seqs.length + " text units..\n")
    seqs
  }

  private def updateFreqs(freqs: ArrayBuffer[Int], text: Array[Array[Int]]) = {
    val tl = text.length
    val curSize = freqs.size
    val diff = symbolTable.size - curSize
    var i = 0; while (i < diff) { freqs append 0; i += 1 }
    i = 0;
    while (i < tl) {
      var j = 0
      val sl = text(i).length
      val seg = text(i)
      while (j < sl) {
        val t = seg(j)
        freqs(t) += 1
        j += 1
      }
      i += 1
    }
  }

  private def getFreqs(text: Array[Array[Int]]): Array[Int] = {
    val tl = text.length
    val fs = symbolTable.size
    val freqs = Array.fill(fs)(0)
    var i = 0
    while (i < tl) {
      var j = 0
      val sl = text(i).length
      val seg = text(i)
      while (j < sl) {
        val t = seg(j)
        freqs(t) += 1
        j += 1
      }
      i += 1
    }
    freqs
  }

  private def getContextPhrases(text: Array[Array[Int]], offset: Int) = {
    val fs = symbolTable.size
    val context = Array.tabulate(fs) { _ => new ArrayBuffer[Int] }
    var j = 0
    val tl = text.length
    val ioff = if (offset < 0) -offset else 0
    while (j < tl) {
      var i = ioff
      val sl = math.min(text(j).length, (text(j).length - offset))
      val seg = text(j)
      while (i < sl) {
        val ct = seg(i + offset)
        val t = seg(i)
        context(t) append ct
        i += 1
      }
      j += 1
    }
    val oContext = Array.tabulate(fs) { i => context(i).toList }
    oContext
  }

  private def updateContextPhrases(context: ArrayBuffer[HistoGram], text: Array[Array[Int]], offset: Int) = {
    val tl = text.length
    val curSize = context.size
    val diff = symbolTable.size - curSize
    var i = 0; while (i < diff) { context append (new HistoGram()); i += 1 }
    var j = 0
    val ioff = if (offset < 0) -offset else 0
    while (j < tl) {
      var i = ioff
      val sl = math.min(text(j).length, (text(j).length - offset))
      val seg = text(j)
      while (i < sl) {
        val ct = seg(i + offset)
        val t = seg(i)
        context(t).update(ct)
        i += 1
      }
      j += 1
    }
  }

  private def getFreqOrderedTerms(freqs: Array[Int]) = {
    val tl = freqs.length
    val freqOrdered = Array.tabulate(tl)(i => i)
    freqOrdered.sortWith { (l, r) => freqs(l) > freqs(r) }
  }

  private def computeS1(s: Int) = {
    var q = 0.0
    var j = 0; while (j < ncells) {
      if (cell2cluster(j) != -1) q += biQ2(s, j)
      j += 1
    }
    q
  }

  @inline
  private def computeL2(s: Int, t: Int): Double = {
    var l = computeS1(s) + computeS1(t) - biQ2(s, t)
    var u = 0
    while (u < ncells) {
      if (cell2cluster(u) != -1 && u != s && u != t) l -= biHypQ2(s, t, u)
      u += 1
    }
    l -= hypQ2(s, t)
    l
  }

  @inline
  private def computeL2(): Unit = {
    var i = 0; while (i < ncells) {
      if (cell2cluster(i) != -1) {
        val l2_i = l2(i)
        var j = 0; while (j < ncells) {
          if (cell2cluster(j) != -1)
            if (validClusterPair(i, j)) l2_i(j) = computeL2(i, j)
          j += 1
        }
      }
      i += 1
    }
  }

  @inline
  private def computeL2WithOld(s: Int, t: Int, u: Int, v: Int, w: Int) = {
    var l = l2(v)(w)
    l -= biQ2(v, s) + biQ2(w, s) + biQ2(v, t) + biQ2(w, t)
    l += biHypQ2(v, w, s) + biHypQ2(v, w, t)
    l += biQ2(v, u) + biQ2(w, u)
    l -= biHypQ2(v, w, u)
    l
  }

  private def addNewPhrase(a: Int) = {

    val freqs = clusterData.get.freqs
    val rCntxt = clusterData.get.rCntxt
    val lCntxt = clusterData.get.lCntxt

    val s = addClusterToFreeCell(a)
    initCell(s)
    cluster2rep += (a -> a)
    rep2cluster += (a -> a)

    p1(s) = freqs(a).toDouble / textSize
    var cfreqs = collection.immutable.IntMap[Int]()
    rCntxt(a).hist foreach { case (phr,cnt) =>
      var rphr = phrase2rep.getRoot(phr) // get rep phrase
      if (rep2cluster.contains(rphr)) {
        rphr = rep2cluster(rphr)
        if (cluster2cell.contains(rphr)) {
          val v = cfreqs.get(rphr).getOrElse(0)
          cfreqs = cfreqs.updated(rphr, (v + cnt))
        }
      }
    }
    cfreqs foreach { case (b, cnt) => cur_mi += setP2Q2(cluster2cell(a), cluster2cell(b), cnt) }
    cfreqs = collection.immutable.IntMap[Int]()
    lCntxt(a).hist foreach { case (phr,cnt) =>
      var rphr = phrase2rep.getRoot(phr) // get rep phrase
      if (rep2cluster.contains(rphr)) {
        rphr = rep2cluster(rphr)
        if (cluster2cell.contains(rphr)) {
          val v = cfreqs.get(rphr).getOrElse(0)
          cfreqs = cfreqs.updated(rphr, (v + cnt))
        }
      }
    }
    cfreqs foreach { case (b, cnt) => cur_mi += setP2Q2(cluster2cell(b), cluster2cell(a), cnt) }
    cur_mi -= q2(s)(s)
    var i = 0; while (i < ncells) {
      if (s != i && (cell2cluster(i) != -1)) {
        val (bs, bi) = if (validClusterPair(s, i)) (s, i) else (i, s)
        l2(bs)(bi) = computeL2(bs, bi)
      }
      i += 1
    }
    l2Par foreach {
      case (l2_i, i) =>
        if (s != i && (cell2cluster(i) != -1)) {
          var j = 0; while (j < ncells) {
            if (cell2cluster(j) != -1 && validClusterPair(i, j))
              l2_i(j) += biQ2(i, s) + biQ2(j, s) - biHypQ2(i, j, s)
            j += 1
          }
        }
    }
  }

  private def mergeClusters(s: Int, t: Int): Unit = {
    val a = cell2cluster(s)
    val b = cell2cluster(t)

    curClusterIdx += 1
    val c = curClusterIdx
    val u = addClusterToFreeCell(c)
    freeCells(s, t)

    clusterTree += (c -> (a, b))
    cur_mi -= l2(s)(t)
    val ba = cluster2rep(a)
    val bb = cluster2rep(b)
    phrase2rep.join(ba, bb)
    val bc = phrase2rep.getRoot(ba) // new rep phrase for new cluster (of a and b merged)
    cluster2rep -= a
    cluster2rep -= b
    rep2cluster -= ba
    rep2cluster -= bb
    cluster2rep += (c -> bc)
    rep2cluster += (bc -> c)
    p1(u) = p1(s) + p1(t)
    p2(u)(u) = hypP2(s, t)
    q2(u)(u) = hypQ2(s, t)
    var v = 0; while (v < ncells) {
      if (v != u && (cell2cluster(v) != -1)) {
        p2(u)(v) = hypP2_1(s, t, v)
        p2(v)(u) = hypP2_2(v, s, t)
        q2(u)(v) = hypQ2_1(s, t, v)
        q2(v)(u) = hypQ2_2(v, s, t)
      }
      v += 1
    }

    l2Par foreach {
      case (l2_v, v) =>
        if (cell2cluster(v) != -1) {
          var w = 0; while (w < ncells) {
            if (!(cell2cluster(w) < 0) && validClusterPair(v, w))
              l2_v(w) = if ((v == u) || (w == u)) computeL2(v, w) else computeL2WithOld(s, t, u, v, w)
            w += 1
          }
        }
    }
  }

  private def mergeClusters(ip: (Int, Int)): Unit = mergeClusters(ip._1, ip._2)

  private def findBestClusterPair(): (Int, Int) = {
    var bestS = -1
    var bestT = -1
    var minL = 1E300
    var s = 0; while (s < ncells) {
      if (!(cell2cluster(s) < 0)) {
        var t = 0; while (t < ncells) {
          if (!(cell2cluster(t) < 0) && validClusterPair(s, t)) {
            val l = l2(s)(t)
            if (l < minL) {
              minL = l
              bestS = s
              bestT = t
            }
          }
          t += 1
        }
      }
      s += 1
    }
    //println("found best cluster pair: " + bestS + " and " + bestT)
    (bestS, bestT)
  }

  def executeClustering() = {
    val n = symbolTable.size
    println("... preparing to cluster " + n + " unique vocabulary items")
    val freqOrdered = clusterData.get.freqOrdered
    computeL2()
    curClusterIdx = n // start indexing clusters after current phrase
    var dd = 0
    var st = System.nanoTime
    var i = initC; while (i < n) {
      val na = freqOrdered(i)
      addNewPhrase(na)
      mergeClusters(findBestClusterPair())
      if ((dd > 0) && (dd % 500) == 0) {
        val p = st
        st = System.nanoTime
        println("... 500 lexical items clustered (in " + ((st - p) / 1E9) + " seconds) -- total clustered: " + dd)
      }
      dd += 1
      i += 1
    }
    secondStageId = curClusterIdx
    println("\nSecond stage clustering...")
    i = 0; while (i < (initC - 1)) {
      mergeClusters(findBestClusterPair())
      i += 1
    }
  }

  case class El(vl: Int, pth: String)

  def outputClusters(os: java.io.OutputStreamWriter, wprop: Boolean) = {
    def traverse(els: Stack[El]): Unit =
      if (!els.isEmpty) {
        val el = els.pop
        val a = el.vl
        clusterTree.get(a) match {
          case Some(tr) =>
            val ext = a >= secondStageId
            els push El(tr._2, (if (ext) el.pth + '1' else el.pth))
            els push El(tr._1, (if (ext) el.pth + '0' else el.pth))
            traverse(els)
          case None =>
            if (wprop) {
              os.write(revTable(a))
              os.write(' ')
              os.write(el.pth)
              os.write('\n')
            } else {
              os.write(el.pth)
              os.write('\t')
              os.write(revTable(a))
              os.write('\n')
            }
            traverse(els)
        }
      }
    traverse(new Stack() push (El(cluster2cell.head._1, "")))
  }

}

object BrownClustering {

  def main(args: Array[String]) = {
    val opts = new ClusteringOptions(args)
    val bc = new BrownClustering(opts.nc, opts.txt, verbose = opts.verbose)
    opts.idir match {
      case Some(d) => bc.getClusterData2(new java.io.File(d), true)
      case None =>
        opts.ifile match {
          case Some(f) => bc.getClusterData2(new java.io.File(f), false)
          case None => throw new RuntimeException("\nInput File or Directory must be provided\n")
        }
    }
    if (!opts.ingestOnly) {
      println("\n..finished reading data")
      bc.createInitialClusters()
      println("..finished creating initial clusters")
      bc.executeClustering()
      val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(new java.io.File(opts.ofile))), "UTF-8")
      bc.outputClusters(os, opts.prop)
      os.flush
      os.close
    }
  }
}

class ClusteringOptionHandler(params: Array[String]) extends CommandLineHandler {
  def process(): Unit = {
    process(params.toList)
  }

  "--input-file" desc "Input file"
  "--input-dir" desc "Input directory"
  "--num-clusters" desc "Number of leaf nodes in clustering"
  "--output-file" desc "Output file containing dendrogram"
  "--quiet" flag "Print minimal output while running"
  "--property-format" flag "Print output in word property format"
  "--text-input" flag "Process plain text (rather than parsing tags)"
  "--verbose" flag "Provide verbose output"
  "--ingest-only" flag "For memory profiling, only read-in data, do not cluster"
}

class ClusteringOptions(val argv: Array[String], val optHandler: ClusteringOptionHandler) {
  def this(argv: Array[String]) = this(argv, new ClusteringOptionHandler(argv))
  optHandler.process()

  val ifile = optHandler.get("--input-file")
  val idir = optHandler.get("--input-dir")
  val nc = optHandler.get("--num-clusters").get.toInt
  val ofile = optHandler.get("--output-file").get
  val quiet = optHandler.check("--quiet")
  val prop = optHandler.check("--property-format")
  val txt = optHandler.check("--text-input")
  val verbose = optHandler.check("--verbose")
  val ingestOnly = optHandler.check("--ingest-only")

}

class UnionSet {

  var parent: Array[Int] = Array()
  def join(u: Int, v: Int) = exec(u, v, true)
  def inSameSet(u: Int, v: Int) = exec(u, v, false)
  def init(n: Int) = {
    parent = Array.tabulate(n) { n => n }
  }

  def exec(u: Int, v: Int, doIt: Boolean): Boolean = {
    val ru = getRoot(u)
    val rv = getRoot(v)
    if (ru == rv) true
    else {
      if (doIt) parent(ru) = rv
      false
    }
  }

  def getRoot(v: Int): Int = {
    var rv = v
    while (parent(rv) != rv) rv = parent(rv)
    var cv = v
    while (cv != rv) {
      val pv = parent(cv)
      parent(cv) = rv
      cv = pv
    }
    rv
  }
}

object UnionSet {
  def apply() = new UnionSet
  def apply(i: Int) = {
    val us = new UnionSet
    us.init(i)
    us
  }
}
