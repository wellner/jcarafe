/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.crf

import sbinary._

object InstanceSerializations extends DefaultProtocol {

  implicit def crfInstMap : Format[CrfInstance] = {
    asProduct4((l: Int, o:Int, si: Int, farr: Array[Array[Feature]]) => 
      new CrfInstance(l,o,si,Some(farr)) : CrfInstance)((ci: AbstractInstance) => (ci.label, ci.orig, ci.segId, ci.getCompVec))
  }
  
  implicit def nonFactoredCrfInstMap : Format[NonFactoredCrfInstance] = {
    asProduct4((l: Int, o:Int, si: Int, farr: Array[Array[Feature]]) => 
      new NonFactoredCrfInstance(l,o,si,Some(farr)) : NonFactoredCrfInstance)((ci: AbstractInstance) => (ci.label, ci.orig, ci.segId, ci.getCompVec))
  }

  implicit def featureMap : Format[Feature] = {
    asProduct5((prv: Int, cur: Int, fid: Int, nfid: Int, vl: Double) => 
      new NBinFeature(vl,prv, cur, fid, nfid) : Feature)((f: Feature) => (f.prv, f.cur, f.fid, f.nfid, f.value))
  }
  
  implicit def seqMap : Format[Seq[CrfInstance]] = {
    wrap[Seq[CrfInstance],List[CrfInstance]](_.toList,{(s:List[CrfInstance]) => s.toIndexedSeq : Seq[CrfInstance]})
  }
  
  implicit def nfseqMap : Format[Seq[NonFactoredCrfInstance]] = {
    wrap[Seq[NonFactoredCrfInstance],List[NonFactoredCrfInstance]](_.toList,{(s:List[NonFactoredCrfInstance]) => s.toIndexedSeq : Seq[NonFactoredCrfInstance]})
  }

}
