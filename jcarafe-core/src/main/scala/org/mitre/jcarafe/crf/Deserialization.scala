/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf


abstract class Deserialization {

  def getSlice(s: Int, e: Int) : Deserialization

}
