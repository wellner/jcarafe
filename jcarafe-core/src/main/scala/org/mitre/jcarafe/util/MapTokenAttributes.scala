package org.mitre.jcarafe.util

import org.mitre.jcarafe.tokenizer.{Element, Tag, FastTokenizer}

object MapTokenAttributes {
  import ProjectAlignedAttributes._

  def mapPos(s: String) = s match {
    case "PUNCT" => "PUNCT"
    case "CC" => "CONJ"
    case "CD" => "NUM"
    case "CD|RB" => "NUM"
    case "DT" => "DET"
    case "EX" => "DET"
    case "FW" => "X"
    case "IN" => "ADP"
    case "IN|RP" => "ADP"
    case "JJ" => "ADJ"
    case "JJR" => "ADJ"
    case "JJRJR" => "ADJ"
    case "JJS" => "ADJ"
    case "LS" => "X"
    case "MD" => "VERB"
    case "NN" => "NOUN"
    case "NNP" => "NOUN"
    case "NNS" => "NOUN"
    case "NNPS" => "NOUN"
    case "NP" => "NOUN"
    case "PDT" => "DET"
    case "POS" => "PRT"
    case "PRP" => "PRON"
    case "PRP$" => "PRON"
    case "PRT" => "PRT"
    case "RB" => "ADV"
    case "RBR" => "ADV"
    case "RBS" => "ADV"
    case "RN" => "X"
    case "RP" => "PRT"
    case "SYM" => "X"
    case "TO" => "PRT"
    case "UH" => "X"
    case "VB" => "VERB"
    case "VBD" => "VERB"
    case "VBG" => "VERB"
    case "VBN" => "VERB"
    case "VBP" => "VERB"
    case "VBZ" => "VERB"
    case "VP" => "VERB"
    case "WDT" => "DET"
    case "WH" => "X"
    case "WP" => "PRON"
    case "WP$" => "PRON"
    case "WRB" => "ADV"
    case _ => "NOUN"
  }

  def mapToken(t: Token): Token = {
    val mp = t.props
    mp.get("pos") match {
      case Some(pvl) =>
        val npvl = pvl map { pv =>
          mapPos(pv.vl)
        }
        new Token(Map("pos" -> npvl.head), t.tokVal)
      case None => t
    }
  }
  
  def getSentTokens(elems: List[Element], stck: List[Element]) : (List[Element],List[Element]) = elems match {
    case Tag("</s>",false) :: r => (elems,stck.reverse)
    case Nil => (Nil,stck.reverse)
    case t :: r => getSentTokens(r,t :: stck)
  }
  
  def gatherLogicalTokensBySentence(elems: List[Element], stck: List[List[Token]]): List[List[Token]] = {
    elems match {
      case Tag("<s>",true) :: r => 
        val (nr,stoks) = getSentTokens(elems,Nil)
        val ts = gatherLogicalTokens(stoks) map mapToken
        gatherLogicalTokensBySentence(nr,(ts :: stck))
      case v :: r => gatherLogicalTokensBySentence(r,stck)
      case Nil => stck.reverse
    }
  }


  def main(args: Array[String]) = {
    val outFile = new java.io.File(args(1))
    val ts = FastTokenizer.parseFile(args(0), true)
    val toks = gatherLogicalTokensBySentence(ts, Nil)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(outFile)), "UTF-8")
    toks foreach {sent =>
      os.write("<s>")
      sent foreach {t => os.write(t.tokenBestOverToString(0.9,false)) }
      os.write("</s>\n\n")
      }
    os.close()
  }
}