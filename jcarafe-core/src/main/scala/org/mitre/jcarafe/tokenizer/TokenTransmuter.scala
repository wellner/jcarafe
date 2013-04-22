package org.mitre.jcarafe.tokenizer

import java.io.File

abstract class TokenTransmuter {
  // idea here is for set of tools that remain training data in various ways to create semi-synthetic training sets

  def transmuteFile(inFile: File, outFile: File) : Unit
  def elementSequenceToFile(els: Seq[Element], outFile: File) : Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(outFile)), "UTF-8")
    els foreach {e => os.write(e.getString)}
    os.close()
  }
}


class TokenTagUpcaseTransmuter(val tagTypes: Set[String]) {
  
  def getMappedTokens(els: List[Element]) : (List[Element],List[Element]) = {
    els match {
      case Tag(_,false) :: r => (Nil,els)
      case Tok(s) :: r => 
        val (nts,nr) = getMappedTokens(r)
        (Tok(s.toUpperCase()) :: nts, nr) 
      case a :: r => 
        val (nts, nr) = getMappedTokens(r)
        (a :: nts, nr)
      case Nil => (Nil,Nil)
    }
  }
  
  def transmuteFile(inFile: File, outFile: File) : Unit = {
    val toks = FastTokenizer.parseFile(inFile.getPath())
    def upcaseTokensWithinTags(rToks: List[Element], acc: List[Element]) : List[Element] = {
      rToks match {
        case Tag(s,b) :: r if tagTypes.contains(s) =>
          val (nts,nr) = getMappedTokens(r)
          upcaseTokensWithinTags(nr,(nts.reverse ::: acc))
        case a :: r => upcaseTokensWithinTags(r,a :: acc)
        case Nil => acc.reverse
      }
    }
  }
}

object TokenTagUpcaseTransmuter {
  def main(args: Array[String]) = {
    val inFile = new File(args(0))
    val outFile = new File(args(1))
    val tags = for (i <- 2 until args.length) yield args(i)
    val transmuter = new TokenTagUpcaseTransmuter(tags.toSet)
    transmuter.transmuteFile(inFile,outFile)
  }
}