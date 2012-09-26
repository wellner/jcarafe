/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.util


sealed abstract class JsonType
case class JsObject(val obj: Map[String,JsonType]) extends JsonType
case class JsString(val s: String) extends JsonType
case class JsArray(val arr: List[JsonType]) extends JsonType
case class JsInt(val i: Int) extends JsonType
case class JsDouble(val i: Double) extends JsonType
case object JsTrue extends JsonType
case object JsFalse extends JsonType
case object JsNull extends JsonType

object Json {
  
  val mapper = new org.codehaus.jackson.map.ObjectMapper
  
  def buildJson(x: Any) : JsonType = x match {
    case vl: String => JsString(vl)
    case vl: Int => JsInt(vl)
    case vl: Double => JsDouble(vl)
    case vl: java.util.ArrayList[Any] =>
      val lb = new scala.collection.mutable.ListBuffer[Any]
      var i = 0
      while (i < vl.size) {
        lb += vl.get(i)
        i += 1
      }
      val elements = lb.toList
      JsArray(elements map buildJson)
    case vl: java.util.HashMap[String,Any] =>
      val lb = new scala.collection.mutable.ListBuffer[String]
      val arr = vl.keySet.toArray
      var i = 0
      while (i < arr.length) {
        val v : String = arr(i).toString
        lb += v
        i += 1
      }
      val arKeys : List[String] = lb.toList 
      val elements = arKeys.foldLeft(Map():Map[String,JsonType]) {case (ac,k) => ac + (k -> buildJson(vl.get(k)))}
      JsObject(elements)
    case true => JsTrue
    case false => JsFalse
    case _ => JsNull
  }	
  
  def constructJsonTypeOfRawString(s: String) : JsonType = JsString(s)
  def constructJsonTypeOfString(s: String) : JsonType = {
    val is = new java.io.ByteArrayInputStream(s.getBytes("UTF-8"))
    val r = buildJson(mapper.readValue(is, classOf[java.util.HashMap[String,Any]]))
    is.close
    r
  }
  def constructJsonTypeOfTokenSeq(s: Seq[String]) : JsonType = {
    val jts = s map constructJsonTypeOfRawString
    JsArray(jts.toList)
  }

  def constructJsonType(s: String) : JsonType = constructJsonType(new java.io.File(s))
  def constructJsonType(f: java.io.File) : JsonType = {
    val is = new java.io.InputStreamReader(new java.io.BufferedInputStream(new java.io.FileInputStream(f)), "UTF-8")
    val r = buildJson(mapper.readValue(is, classOf[java.util.HashMap[String,Any]]))
    is.close
    r
  }

  def writeJson(obj: JsonType, s: String) : Unit = writeJson(obj,new java.io.File(s))
  def writeJson(obj: JsonType, file: java.io.File) : Unit = {
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(file)), "UTF-8")
    writeJson(obj,os)
    os.close
  }

  def writeJson(obj: JsonType, writer: java.io.Writer) : Unit = {
    val f = new org.codehaus.jackson.JsonFactory()
    val g = f.createJsonGenerator(writer)
    def sendJson(obj: JsonType) : Unit = obj match {
      case JsObject(v) => 
        g.writeStartObject()
        v foreach {case (s,obj) => g.writeFieldName(s); sendJson(obj) }
        g.writeEndObject()
      case JsString(v) => g.writeString(v)
      case JsInt(v) => g.writeNumber(v)
      case JsTrue => g.writeBoolean(true)
      case JsFalse => g.writeBoolean(false)
      case JsDouble(v) => g.writeNumber(v)
      case JsNull => g.writeNull()
      case JsArray(v) => 
        g.writeStartArray()
        v foreach { sendJson }
        g.writeEndArray()
    }
    sendJson(obj)
    g.flush()
    g.close()
  }	

  def writeJsonToString(obj: JsonType) : String = {
    val sout = new java.io.StringWriter
    val f = new org.codehaus.jackson.JsonFactory()
    val g = f.createJsonGenerator(sout)
    def sendJson(obj: JsonType) : Unit = obj match {
      case JsObject(v) => 
        g.writeStartObject()
        v foreach {case (s,obj) => g.writeFieldName(s); sendJson(obj) }
        g.writeEndObject()
      case JsString(v) => g.writeString(v)
      case JsInt(v) => g.writeNumber(v)
      case JsTrue => g.writeBoolean(true)
      case JsFalse => g.writeBoolean(false)
      case JsDouble(v) => g.writeNumber(v)
      case JsNull => g.writeNull()
      case JsArray(v) => 
        g.writeStartArray()
        v foreach { sendJson }
        g.writeEndArray()
    }
    sendJson(obj)
    g.flush()
    g.close()
    sout.toString
  }
}
