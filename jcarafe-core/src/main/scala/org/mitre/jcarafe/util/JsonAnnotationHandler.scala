/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */
package org.mitre.jcarafe.util

object JsonAnnotationHandler {

  /*
   * This handles getting the attribute labels whether using MAT v < 2.0 or the
   * newer MAT 2.0 or > schema.
   */
  def getAttributeLabels(attrList: List[JsonType]): List[String] = {
    var atts: List[String] = Nil
    attrList foreach { jobj =>
      jobj match {
        case JsString(s) => atts = s :: atts // old schema - all strings here are the attribute values
        case JsObject(obj) => // NEW scheme - objects here indicate name of attribute value and attribute type (which is ignored here)
          obj.get("name") foreach { case JsString(s) => atts = s :: atts case _ => }
        case _ =>
      }
    }
    atts
  }
  /*
   * This gathers annotations over the MAT-JSON "defined" schema.
   */
  def getAnnotations(signal: Option[String], js: JsonType, tagset: Tagset, asPreProc: Boolean = false, justLabel: Boolean = false, toks: Boolean = false): List[Annotation] =
    js match {
      case JsArray(arr) =>
        val asetsOfTargetType =
          arr.filter {
            case JsObject(o) =>
              val s = o("type") match { case JsString(s) => s case _ => "" }
              val hasId = o.get("hasID") match { case Some(JsTrue) => true case _ => false }
              val hasSpan = o.get("hasSpan") match { case Some(JsFalse) => false case _ => true }
              o("attrs") match {
                case JsArray(ar_p) =>
                  val ar = getAttributeLabels(ar_p)
                  if (ar.length > 0) {
                    tagset.set.exists { al =>
                      al match {
                        case Label(l, avs) =>
                          val t = (l == s && (justLabel || avs.forall { case (k, v) => ar.contains(k) }))
                          t
                        case SLabel(l) if ((l == s)) => true // select annotations ignoring attributes
                        case _ => false
                      }
                    }
                  } else tagset.set.exists { al => al match { case SLabel(l) => l == s case _ => false } }
                case _ => tagset.set.exists { al => al match { case SLabel(l) => l == s case _ => false } }
              }
            case _ => false
          }
        asetsOfTargetType flatMap { el =>
          el match {
            case JsObject(o) =>
              val s = o("type") match { case JsString(s) => s case a => throw new RuntimeException("No valid type value: " + a) }
              val s_attsKeys = o("attrs") match { case JsArray(ar_p) => getAttributeLabels(ar_p) case _ => Nil }
              val s_attsKey = s_attsKeys match { case s :: _ => Some(s) case Nil => None }
              o("annots") match {
                case JsArray(arr) =>
                  val annotBuf = new scala.collection.mutable.ListBuffer[Annotation]
                  arr.foreach { el =>
                    el match {
                      case JsArray(JsInt(st) :: JsInt(en) :: Nil) =>
                        val vl = signal match { case Some(sig) => Some(sig.substring(st, en)) case None => None }
                        annotBuf += new Annotation(st, en, false, SLabel(s), vl)
                      case JsArray(JsInt(st) :: JsInt(en) :: attvlsP) => // case where annotation has an attribute
                        val attvls: List[String] = attvlsP map { case JsString(s1) => s1 case _ => "--" }
                        val attVlMap = (s_attsKeys zip attvls).foldLeft(Map(): Map[String, String]) { _ + _ }
                        var attC: Option[String] = None
                        var valC: String = ""
                        val vl = signal match { case Some(sig) => Some(sig.substring(st, en)) case None => None }
                        if (asPreProc) {
                          annotBuf += new Annotation(st, en, false, SLabel(s), vl, Some(attVlMap))
                        } else {
                          tagset.set.foreach { al: AbstractLabel =>
                            al match {
                              case Label(_, _) =>
                                al.hasValue(attvls) match { // in this case, just check whether attribute value matches a provided label
                                  case Some((k, v)) =>
                                    attC = Some(k)
                                    valC = v
                                  case None =>
                                }
                                // now check for attribute value wildcards
                                s_attsKeys foreach { a =>
                                  al.hasAttributeWithWildCard(a) match {
                                    case Some((k, _)) =>
                                      attC = Some(k)
                                      valC = attVlMap.get(k).getOrElse("--")
                                    case None =>
                                  }
                                }
                                if (justLabel) // when only selecting on the label, add in attribute key and current value
                                  attvls match {
                                    case a :: Nil =>
                                      attC = s_attsKey; valC = a
                                    case _ => annotBuf += new Annotation(st, en, false, SLabel(s), vl, None)
                                  }
                                attC match {
                                  case Some(k) =>
                                    if (justLabel)
                                      annotBuf += new Annotation(st, en, false, SLabel(s), vl, Some(Map(k -> valC)))
                                    else
                                      annotBuf += new Annotation(st, en, false, Label(s, Map(k -> valC)), vl)
                                  case None =>
                                }
                              case SLabel(sp) =>
                                if (sp == s)
                                  annotBuf += new Annotation(st, en, false, SLabel(s), vl, None)
                              case _ =>
                            }
                          }
                        }
                      case a => throw new RuntimeException("Unexpected annotation" + a)
                    }
                  }
                  annotBuf.toList
                case _ => throw new RuntimeException("Expected annots")
              }
            case _ => throw new RuntimeException("Expected array of annotations")
          }
        }
      case _ => throw new RuntimeException("Expected array of annotations")
    }

}
