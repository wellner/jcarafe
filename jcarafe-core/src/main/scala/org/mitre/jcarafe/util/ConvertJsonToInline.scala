package org.mitre.jcarafe.util

import java.io.File
import collection.mutable.{Stack,Queue}

object ConvertJsonToInline {

  def convertFile(json: File, inline: File, tagsetFile: File) = {
    val tagset = Tagset.loadTagset(tagsetFile.getPath)
    val js = Json.constructJsonType(json)
    val asets = js match {case JsObject(o) => try {o("asets")} catch {case _ => new JsArray(Nil)} case _ => new JsArray(Nil)}
    val sig = js match { 
      case JsObject(o) => 
        o("signal") match {case JsString(s) => s case _ => throw new RuntimeException("Expected signal to be a string")}
      case _ => throw new RuntimeException("No signal found")}
    
    val annots : Queue[Annotation] = new Queue
    JsonAnnotationHandler.getAnnotations(None,asets,tagset).sorted foreach {v => annots += v}
    val o = new java.io.FileOutputStream(inline)
    val os = new java.io.OutputStreamWriter(o)
    val sigArr = sig.toCharArray()
    val numChars = sigArr.length
    val toEnd : Stack[Annotation] = new Stack  
    for (i <- 0 until numChars) {
      // check for end tags to emit
      if (!toEnd.isEmpty && i == toEnd.top.en) {
        val ts = toEnd.pop
        os.write(ts.typ.labelTag(true))
      }
      if (!annots.isEmpty && i == annots.head.st) {
        val ts = annots.dequeue()
        toEnd push ts
        os.write(ts.typ.labelTag(false))
      }
      os.write(sigArr(i))
    }
    os.close()
    o.close()
  }
  
  def main(args: Array[String]) = {
    convertFile(new File(args(0)), new File(args(1)), new File(args(2)))
  }
}

