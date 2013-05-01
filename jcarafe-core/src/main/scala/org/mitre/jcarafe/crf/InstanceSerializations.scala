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
  
  
  /*
  implicit def crfInstMap : Format[CrfInstance] = {
    asProduct4({(l: Int, o:Int, si: Int, farr: Set[CrfInstance#FType]) =>
      val ci = new CrfInstance(l,o,si,None)
      farr foreach ci.add
      ci : CrfInstance}){(ci: AbstractInstance) => (ci.label, ci.orig, ci.segId, ci.userFeatures)}
  }
  *
  */
          
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
  
  implicit def stringSourceSeqMap : Format[SourceSequence[String]] = {
    asProduct4((ss: Seq[ObsSource[String]], par: Option[SourceSequence[String]], st: Int, en: Int) =>
      new SourceSequence(ss,par,st,en)){(ss: SourceSequence[String]) => (ss.seq, ss.parentSeq, ss.st, ss.en)}
  }
  
  implicit def seqSourceMap : Format[Seq[ObsSource[String]]] = {
    wrap[Seq[ObsSource[String]],List[ObsSource[String]]](_.toList,{(s:List[ObsSource[String]]) => s.toIndexedSeq})
  }
  
  implicit def obsSourceMap : Format[ObsSource[String]] = {
    asProduct4((lb: Int, obs: String, bg: Boolean, info: Option[Map[String,String]]) => 
      new ObsSource[String](lb, obs, bg, info)){(os: ObsSource[String]) => (os.label,os.obs.toString,os.beg,os.info)}
  }
  
}
