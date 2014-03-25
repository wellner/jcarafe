/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */
package org.mitre.jcarafe.dparser

import org.mitre.jcarafe.crf.{AbstractInstance,Feature,DenseTrainable,CoreModel,AccessSeq,DecodingAlgorithm,InstanceSequence}
import org.mitre.jcarafe.util.FastLoops._
import org.mitre.jcarafe.jama._


abstract class MstCrf(val nfs: Int, val gPrior: Double = 100.0) extends DenseTrainable[AbstractInstance] {

  import org.mitre.jcarafe.util.FastLoops._

  /**
   * These are the model parameters
   */
  val lambdas: collection.mutable.IndexedSeq[Double] = Array.fill(nfs)(0.0)
  val numParams = nfs
  val gradient: Array[Double] = Array.fill(nfs)(0.0)
  val invSigSqr = 1.0 / gPrior

  def initialize() = {}
  def getCoreModel() = new CoreModel(lambdas,numParams,0,0,0)

  def regularize() = {
    var i = 0
    var llMod = 0.0
    val llen = lambdas.length
    while (i < llen) {
      val li = lambdas(i)
      gradient(i) = li * invSigSqr
      llMod -= (li * li * invSigSqr) / 2.0
      i += 1
    }
    llMod
  }

  def printMatrix(m: Array[Array[Double]]) = {
    println("")
    m foreach {r => r foreach {e => printf(" %f",e)}; println("")}
    println("")
  }

  protected def inferGradientAndLL(iseq:Seq[AbstractInstance]) = {
    var sll = 0.0
    var i = 0
    val sl = iseq.length 
    val kirchoff = Array.fill(sl,sl)(0.0)
    val sMat = Array.fill(sl,sl)(0.0)

    forIndex(sl){i =>
      val instFeatures = iseq(i).getCompVec(0)
      val label = iseq(i).label
      updateScores(sMat,instFeatures,lambdas,i,true,label)
    }
    computeKirchoff(sl,kirchoff,sMat)

    val luDecomp = new org.mitre.jcarafe.jama.LUDecomposition(new Matrix(kirchoff))
    val ident = org.mitre.jcarafe.jama.Matrix.identity(sl,sl) 
    val invK = luDecomp.solve(ident).getArray()
    val detK = luDecomp.det()
    val expectations = Array.fill(numParams)(0.0)
    forIndex(sl){i =>
      val instFeatures = iseq(i).getCompVec(0)
      val klen = instFeatures.length
      val label = iseq(i).label
      forIndex(klen){k =>
        val inst = instFeatures(k)
        val parent = inst.cur
        if (parent == label) 
        	sll += lambdas(inst.fid) * inst.value
        val off = if (parent == i) 0.0 else invK(i)(parent)		     
        expectations(inst.fid) += sMat(i)(parent) * inst.value * (invK(i)(i) - off)
        gradient(inst.fid) += sMat(i)(parent) * inst.value * (invK(i)(i) - off)
      }
    }
    sll -= math.log(detK)
    sll
  }

  protected def computeKirchoff(s: Int, km: Array[Array[Double]], sm: Array[Array[Double]]) = {
    forIndex(s){i => // row index
      forIndex(s){j => // column index
      val ss = sm(j)(i)
      if (i != j) km(i)(j) = -ss
      km(i)(i) += sm(i)(j) 
      }
    }
  }

  def getGradient(seqAccessor: AccessSeq[AbstractInstance]) : Option[Double] = {
    var logLi = regularize()
    for (j <- 0 until seqAccessor.length) {
      val seq = seqAccessor(j)
      if (seq.length > 0) logLi += inferGradientAndLL(seq)
    }
    Some(-logLi)           
  }

  protected def updateScores(scoreMat: Array[Array[Double]], instFeatures: Array[Feature], lambdas: collection.mutable.IndexedSeq[Double], pos: Int, takeExp: Boolean, lab: Int) = {
    val klen = instFeatures.length
    forIndex(klen) {k =>
      val inst = instFeatures(k)
      if ((inst.cur) == lab) {
	gradient(inst.fid) -= inst.value // constraint
      }
      scoreMat(pos)(inst.cur) += lambdas(inst.fid) * inst.value
    }
    if (takeExp) {
      val scoreSize = scoreMat.length
      forIndex(scoreSize) {i => scoreMat(pos)(i) = math.exp(scoreMat(pos)(i)) }
    }
  }

}


class MstMAPInference(crf: CoreModel) extends DecodingAlgorithm(crf) {
  
  val lambdas = crf.params

  protected def updateScores(scoreVec: Array[Double], instFeatures: Array[Feature], lambdas: collection.mutable.IndexedSeq[Double], pos: Int) = {
    val klen = instFeatures.length
    val scoreSize = scoreVec.length
    forIndex(klen) {k =>
      val inst = instFeatures(k)
      val parent = inst.cur		    
      scoreVec(parent) += lambdas(inst.fid) * inst.value
    }
    forIndex(scoreSize) {i => scoreVec(i) = math.exp(scoreVec(i)) }
  }
  
  def getCopyOf = new MstMAPInference(this.crf)

  def assignBestSequence(iseq: collection.immutable.IndexedSeq[AbstractInstance]) : Double = {
    val sl = iseq.length
    val sMat = Array.fill(sl,sl)(0.0)
    val t0 = System.nanoTime
    forIndex(sl){i =>
      val instFeatures = iseq(i).getCompVec(0)
      val label = iseq(i).label
      var sVec = sMat(i)		 
      updateScores(sVec,instFeatures,lambdas,i)
    }
    //println("\n score assignment in " + ((System.nanoTime - t0) / 1000000000.0) + " seconds")
    //println("Edge scores: ")
    //forIndex(sMat.length){i => forIndex(sMat.length){j => println("("+i+","+j+") => " + sMat(i)(j))}}
    val t1 = System.nanoTime
    val cle = new ChuLiuEdmonds()
    //println("CLE instance created in " + ((System.nanoTime - t1) / 1000000000.0) + " seconds")
    val t2 = System.nanoTime
    val bstSeq = cle.cleInfer(sMat)
    //println("\n CLE inference performed in" + ((System.nanoTime - t2) / 1000000000.0) + " seconds")
    forIndex(sl){i => iseq(i).label_=(bstSeq(i))}
    0.0
  }
    
} 

class ChuLiuEdmonds {

  case class PNode(val vl: Int, var onStack: Boolean = false, var index: Int = -1, var lowlink: Int = -1, 
		   var nodes: Set[Int] = Set(), var bestIn: Map[Int,Int] = Map(),
		   var parent: Int = -1, var children: Set[Int] = Set()) {
    override def equals(other: Any) = other match {case other: PNode => this.vl == other.vl case _ => false}
    override val hashCode = vl
    override def toString = "PNode("+vl+")"
  }

  var idGensym = 0
  var origSize = 0
  var allNodes : Array[PNode] = Array()
  
  def cleInfer(sMat: Array[Array[Double]]) = {
    val smp = padSMat(sMat)
    initializeGraph(sMat)
    cleAlgorithm((0 until sMat.length).toSet,smp)
    val res = Array.fill(sMat.length)(0)
    forIndex(sMat.length){i => res(i) = allNodes(i).parent}
    res
  }

  def getBestGraph(gr: Set[Int], sm: Array[Array[Double]]) = {
    gr foreach {ni =>
      val v = allNodes(ni)
      var bst = -1
      var bstV = -Double.MaxValue		      
      val pars = sm(ni) 
      forIndex(pars.size) {j =>
        if ((pars(j) >= bstV)) { // subtle - this will ensure we pick the most recently introduced node
	  bstV = pars(j)       
	  bst = j}}
      v.parent = bst
      val pnode = allNodes(bst)
      pnode.children += ni
    }
  }

  def initializeGraph(sMat: Array[Array[Double]]) : Unit = {
    allNodes = Array.tabulate(sMat.length * 2){i => PNode(i)}
    idGensym = sMat.length
    origSize = idGensym
  }

  def padSMat(sMat: Array[Array[Double]]) = {
    val sl = sMat.length
    Array.tabulate(sl * 2){i =>
      Array.tabulate(sl * 2){j =>
	if ((i < sl) && (j < sl)) sMat(i)(j) else 0.0}}
  }

  def getCLESolution(gr: Set[Int], sMat: Array[Array[Double]]) = {
    println("looking for next best graph on: ")
    sMat foreach {row => row foreach {el => println(" " + el)}; println}
    val smp = padSMat(sMat)
    initializeGraph(sMat)
    cleAlgorithm(gr,smp)
    val res = Array.fill(sMat.length)(0)
    var sc = 0.0
    forIndex(gr.size){i => 
      sc += sMat(i)(allNodes(i).parent); res(i) = allNodes(i).parent}
    (res,sc)
  }

  def cleAlgorithm(gr: Set[Int], sMat: Array[Array[Double]]) : Unit = {
    getBestGraph(gr,sMat)
    val sccs = findSCCs(gr)
    println("SCCS: " + sccs)
    val res = sccs.find(_.length > 1)
    res match {
      case None => 
      case Some(cyc) =>
	val lngth = gr.size	
        var bestpars = Array.tabulate(cyc.length){i => ((for (j <- 0 until cyc.length) yield {(sMat(i)(j), j)}).max._2, i)}
        var cbuf : Set[Int] = Set()
        val carr = new collection.mutable.ArrayBuffer[Int]
        bestpars.foreach{case (par,i) => if (!cbuf.contains(par)) {cbuf += par; carr append par}; if (!cbuf.contains(i)) {cbuf += i;carr append i}}
        val c1 : Array[Int]  = carr.toArray
        println("C1: ")
        c1 foreach {x => print(" " + x)}
        println
        val (ng,cycleNodes,cId) = contract(gr,c1,sMat)
        cleAlgorithm(ng, sMat)
        ng foreach {ni =>
	  val n = allNodes(ni)
	  if (ni == cId) { // if this node corresponds to the compacted node we created
	    val par_i = n.parent // this is the parent in compact graph
	    val par = allNodes(par_i)
	    val ind = if (par_i == ni) -1 else par_i
	    println("ind = " + ind)
	    val ri = n.bestIn(ind)
	    println("ri = " + ri)
	    val toset = if (ind < 0) ri else ind
	    println("toset = " + toset)
	    allNodes(ri).parent = toset
	  } else {
	    if (n.parent == cId) {
	      n.bestIn.get(ni) match {
		case Some(par) => n.parent = par
		case None => }
	    }}}
    }
  }

  def contract(graph: Set[Int], cycle: Array[Int], sMat: Array[Array[Double]]) = {
    val cycleSet = cycle.toSet
    var gc = graph.diff(cycleSet) // remove cycle nodes
    val origNodes = gc
    val clen = cycle.length
    val nnode = allNodes(idGensym)
    gc += idGensym
    idGensym += 1
    var totalS = 0.0

    // also set parents here
    forIndex(clen){i =>
      nnode.nodes += cycle(i) 
      val j = if (i > 0) i - 1 else clen - 1
      val n = allNodes(cycle(i))
      n.parent = cycle(j) // set parent here	   
      println("parent of " + cycle(i) + " set to " + cycle(j))
      totalS += sMat(cycle(j))(cycle(i))}

    // add edges out from collapsed cycle node
    origNodes foreach {v =>
      var nn = -Double.MaxValue
      var bst = -1		       
      forIndex(clen){i =>
      	val sc = sMat(v)(cycle(i))
        if (sc > nn) {
	    nn = sc
	    bst = i
	}}
      val vnode = allNodes(v)
      vnode.bestIn += (v -> cycle(bst))	
      sMat(v)(nnode.vl) = nn
    }
    
    // add edges into collapsed cycle node 
    var nn = -Double.MaxValue
    origNodes foreach {v =>
      var bst = -1	
      var snn = -Double.MaxValue		       
      forIndex(clen){i =>		       
	val j = if (i > 0) i - 1 else clen - 1
	val sc = sMat(cycle(i))(v) - sMat(cycle(i))(cycle(j))		       
        if (sc > snn) {
	    bst = i
	    snn = sc
	  }}
      nnode.bestIn += (v -> cycle(bst))
      sMat(nnode.vl)(v) = snn + totalS
      if (snn > nn) nn = snn		       
    }

    // add in root to collapsed cycle node
    var snn = -Double.MaxValue
    var bst = -1
    forIndex(clen){i =>
      val j = if (i > 0) i - 1 else clen - 1
      val sc = sMat(cycle(i))(cycle(i)) - sMat(cycle(i))(cycle(j))
      if (sc > snn) {
	snn = sc
	bst = i
	}}
    nnode.bestIn += ((-1) -> cycle(bst))
    sMat(nnode.vl)(nnode.vl) = (snn + totalS)
    (gc,nnode.nodes,(idGensym - 1))
  }
  
  def findSCCs(graph: Set[Int]) : List[List[Int]] = {
    val sccs = new collection.mutable.ListBuffer[List[Int]]
    var c = 0
    val st = new collection.mutable.Stack[PNode]
    var ind = 0
    val sz = graph.size
    graph foreach {i => allNodes(i).index = -1}
    graph foreach {i => if (allNodes(i).index < 0) tarjan(allNodes(i))}
    
    def tarjan(v: PNode) : Unit = {
      v.index = ind
      v.lowlink = ind
      ind += 1
      st.push(v)
      v.onStack = true // been visited
      v.children foreach {ci =>
	val vp = allNodes(ci)
	if (vp.index < 0) {
	  tarjan(vp)
	  v.lowlink = math.min(v.lowlink, vp.lowlink)
	} else if (vp.onStack) {
	  v.lowlink = math.min(v.lowlink, vp.lowlink)
	}
      }			  
      if (v.lowlink == v.index) {
	val sc = new collection.mutable.ListBuffer[Int]
	var cont = true
	while (cont) {
	  val vp = st.pop()
	  vp.onStack = false
	  sc append vp.vl
	  if (vp.vl == v.vl) cont = false
	} 
	sccs append sc.toList
      }
    }
    sccs.toList
  }
}

class KBestChuLiuEdmonds(val sOrig: Array[Array[Double]]) extends ChuLiuEdmonds {

  class Solution(val sc: Double, val edge: (Int,Int), val res: Array[Int], val mustInclude: Set[(Int,Int)], val mustExclude: Set[(Int,Int)]) 
  implicit def orderedSolution(s: Solution) : Ordered[Solution] = new Ordered[Solution] {
    def compare(other: Solution) = s.sc.compare(other.sc)
  }

  def getScore(sMat: Array[Array[Double]], pars: Array[Int]) = {
    var sc = 0.0
    forIndex(sMat.size){i => sc += sMat(i)(pars(i))}
    sc
  }

  def getLocalScore(sMat: Array[Array[Double]], gr: Set[Int]) = {
    var sc = 0.0
    gr foreach {i => sc += sMat(i)(allNodes(i).parent)}
    sc
  }

  def getNextBestGraph(sm: Array[Array[Double]], gr: Set[Int], r1: Array[Int], sc: Double) = {
    val sMat = padSMat(sm)
    var curBstScore = -Double.MaxValue
    var edge = (-1,-1) // edge to remove
    println("looking for next best graph on: ")
    sm foreach {row => row foreach {el => println(" " + el)}; println}
    gr foreach {ni =>
      val bst = r1(ni)
      sm(ni)(bst) = -1E300
      val (rr,ss) = (new ChuLiuEdmonds).getCLESolution(gr,sm)
      if ((ss < sc) && (ss > curBstScore)) {
	curBstScore = ss
	edge = (ni,bst)
      }
      sm(ni)(bst) = sOrig(ni)(bst) // set it back		
    }
    println("After looking for next edge: ")
    sm foreach {row => row foreach {el => println(" " + el)}; println}
    (edge, (sc - curBstScore))
  }

  def maskWeights(sm: Array[Array[Double]], in: Set[(Int,Int)], ex: Set[(Int,Int)]) = {
    println("Applying mask: " + in + " AND " + ex)
    in foreach {case (i,j) => sm(i)(j) = 1E300}
    ex foreach {case (i,j) => sm(i)(j) = -1E300}
  }

  def unmaskWeights(sm: Array[Array[Double]]) = {
    var i = 0; while (i < sm.length) {
      var j = 0; while (j < sm.length) {
	sm(i)(j) = sOrig(i)(j)
	j += 1}
      i += 1}
  }
 
  def kBestCLEInfer(sMat: Array[Array[Double]], k: Int) = {
    val nnds = (0 until sMat.length).toSet
    val smp = padSMat(sMat)
    initializeGraph(sMat)
    val (bst,bstScore) = (new ChuLiuEdmonds).getCLESolution(nnds,sMat)
    val bstList = new collection.mutable.ListBuffer[Array[Int]]()
    val p_Queue = new collection.mutable.PriorityQueue[Solution]()
    val (e1,d1) = getNextBestGraph(sMat,nnds,bst,bstScore)
    p_Queue += new Solution(bstScore - d1, e1, bst, Set(), Set())
    bstList append bst
    for (i <- 2 to k) {
      val iSol = p_Queue.dequeue
      val yP = iSol.mustInclude + iSol.edge
      val zP = iSol.mustExclude + iSol.edge
      println("before masking: ")
      sMat foreach {row => row foreach {v => print(" " + v)}; println}
      maskWeights(sMat, iSol.mustInclude, zP)
      println("after masking: ")
      sMat foreach {row => row foreach {v => print(" " + v)}; println}
      val (bst_i,bstScore_i) = (new ChuLiuEdmonds).getCLESolution(nnds, sMat)
      bstList append bst_i
      val (e_j,d_j) = getNextBestGraph(sMat, nnds, bst_i, bstScore_i)
      p_Queue += new Solution(iSol.sc - d_j, e_j, bst_i, iSol.mustInclude, zP)

      unmaskWeights(sMat)
      maskWeights(sMat, yP, iSol.mustExclude)

      val (e_i,d_i) = getNextBestGraph(sMat, nnds, bst_i, bstScore_i)
      p_Queue += new Solution(iSol.sc - d_i, e_i, bst_i, yP, iSol.mustExclude)
      unmaskWeights(sMat)

    }
    bstList
  }
}

object KBestChuLiuEdmonds {

  def apply(sm: Array[Array[Double]]) = {
    val copy = Array.tabulate(sm.length){i => Array.tabulate(sm.length){j => sm(i)(j)}}
    new KBestChuLiuEdmonds(copy)
  }

}
