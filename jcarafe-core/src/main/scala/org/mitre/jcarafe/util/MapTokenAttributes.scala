package org.mitre.jcarafe.util

import org.mitre.jcarafe.tokenizer.{ Element, Tag, FastTokenizer }

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
    // Now handle Arabic Treebank tags
    case "NEG" => "ADV"
    case "ABBREV" => "NOUN"
    case _ => "NOUN"
  }

  def mapToken(pr: Double = 1.0)(t: Token): Token = {
    val mp = t.props
    if (util.Random.nextFloat() < pr) {
      mp.get("pos") match {
        case Some(pvl) =>
          val npvl = pvl map { pv =>
            mapPos(pv.vl)
          }
          new Token(Map("pos" -> npvl.head), t.tokVal)
        case None => t
      }
    } else { // in this case, we randomly DROP the part-of-speech
      new Token(Map(), t.tokVal)
    }
  }

  def getSentTokens(elems: List[Element], stck: List[Element]): (List[Element], List[Element]) = elems match {
    case Tag("</s>", false) :: r => (elems, stck.reverse)
    case Nil => (Nil, stck.reverse)
    case t :: r => getSentTokens(r, t :: stck)
  }

  def gatherLogicalTokensBySentence(elems: List[Element], stck: List[List[Token]], pr: Double): List[List[Token]] = {
    elems match {
      case Tag("<s>", true) :: r =>
        val (nr, stoks) = getSentTokens(elems, Nil)
        val ts = gatherLogicalTokens(stoks) map mapToken(pr)
        gatherLogicalTokensBySentence(nr, (ts :: stck), pr)
      case v :: r => gatherLogicalTokensBySentence(r, stck, pr)
      case Nil => stck.reverse
    }
  }

  def main(args: Array[String]) = {
    val outDir = new java.io.File(args(1))
    val tsDir = new java.io.File(args(0))

    val pr = if (args.length > 2) args(2).toDouble else 1.0
    tsDir.listFiles foreach { tsF =>
      val ts = FastTokenizer.parseFile(tsF.getPath(), true)
      val toks = gatherLogicalTokensBySentence(ts, Nil, pr)
      val outFile = new java.io.File(outDir.getPath + "/" + tsF.getName())
      val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(outFile)), "UTF-8")
      toks foreach { sent =>
        os.write("<s>")
        sent foreach { t => os.write(t.tokenBestOverToString(0.9, false)) }
        os.write("</s>\n\n")
      }
      os.close()
    }
  }
}