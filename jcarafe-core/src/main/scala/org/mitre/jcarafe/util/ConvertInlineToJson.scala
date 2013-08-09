package org.mitre.jcarafe.util

import java.io.File
import org.mitre.jcarafe.tokenizer._
import org.mitre.jcarafe.crf.TagParser
import collection.immutable.HashMap

object ConvertInlineToJson {
  
  
  def convertFile(inline: File, jsonOut: File, tagsetFile: File) = {
    val tagset = Tagset.loadTagset(tagsetFile.getPath)
    val signalBuf = new StringBuilder
    val elements = FastTokenizer.parseFile(inline.getPath) // parse/tokenize input and read SGML/XML tags
    var annotTable = new HashMap[AbstractLabel,List[Annotation]]()
    val openStack = new collection.mutable.Stack[(Int,AbstractLabel)]
    var cp = 0
    elements foreach {
      case Tag(tagStr,true) => // start tag
        val al = TagParser.parseString(tagStr)
        if (tagset.isWithin(al))
          openStack push ((cp,al))
      case Tag(tagStr,false) =>
        if (!openStack.isEmpty) {
          val (sp,tp) = openStack.top
          val tstr = "</" + tp.labelHead + ">" // need to construct the END tag that will get pulled out by tokenizer/parser
          if (tstr equals tagStr) {// if this close tag matches abstract label head, it's the corresponding tag
            val curAnnots = annotTable.get(tp).getOrElse(Nil)
            openStack.pop
            annotTable += (tp -> (new Annotation(sp,cp,false,tp) :: curAnnots))             
          }
        }
      case e =>
        val str = e.getString        
        signalBuf ++= str
        cp += str.length
    }
    val jsonPhrases: List[JsonType] =
      (annotTable map {
        case (atyp, annots) =>
          atyp match {
            case SLabel(s) =>
              JsObject(Map("type" -> JsString(s), "attrs" -> JsArray(Nil),
                "annots" -> (JsArray(annots.toList map { a: Annotation => JsArray(List(JsInt(a.st.toInt), JsInt(a.en.toInt))) }))))
            case Label(s, atts) =>
              val keys = (atts map { case (k, v) => k }).toList
              JsObject(Map("type" -> JsString(s), "attrs" -> JsArray(keys map { JsString(_) }),
                "annots" -> (JsArray(annots.toList map { a: Annotation =>
                  a.typ match {
                    case Label(_, atts) =>
                      val attvals = keys map { k => JsString(atts(k)) }
                      JsArray(JsInt(a.st.toInt) :: JsInt(a.en.toInt) :: attvals)
                    case _ => throw new RuntimeException("Incompatible annotation")
                  }
                }))))
            case _ => throw new RuntimeException("Incompatible Annotation")
          }
      }).toList
    val jsonObj = JsObject(Map(
        "signal" -> JsString(signalBuf.toString),
        "asets" -> JsArray(jsonPhrases)))    
    Json.writeJson(jsonObj, jsonOut)
  }
  
  def main(args: Array[String]) = {
    if (args.length != 3) throw new RuntimeException("Utility expects 3 arguments: input file/dir, output file/dir and tagset file")
    val input = new File(args(0))
    val tagFile = new File(args(2))
    if (input.isDirectory()) {
      val outDir = args(1)
      input.listFiles foreach {ifile =>
        val ofile = new File(outDir + "/" + ifile.getName())
        convertFile(ifile, ofile, tagFile)
        }
    }
    else convertFile(input, new File(args(1)), new File(args(2)))
  }

}