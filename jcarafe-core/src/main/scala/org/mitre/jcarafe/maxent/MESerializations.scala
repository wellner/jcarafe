/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.CompactFeature

import sbinary._

object MESerializations extends DefaultProtocol {
  
  implicit def meInstMap : Format[MaxEntInstance] = {
    asProduct3((l: Int, o:Int, farr: Array[CompactFeature]) => new MaxEntInstance(l,o,farr))((me: MaxEntInstance) => (me.label, me.orig, me.getCompactVec))
  }

  implicit def compactFeatureMap : Format[CompactFeature] = {
    asProduct2((v: Double, f: Int) => new CompactFeature(v,f))((c: CompactFeature) => (c.v, c.fid))
  }

}
