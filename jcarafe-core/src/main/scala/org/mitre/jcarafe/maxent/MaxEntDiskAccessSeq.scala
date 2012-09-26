/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.{MemoryAccessSeq, AccessSeq}


class MaxEntDiskAccessSeq(val diskCache: String, val st: Int, val en: Int) extends MaxEntMemoryAccessSeq(Seq()) {
  import MESerializations._

  override def accessSingleInstance(i: Int) = {
    sbinary.Operations.fromFile[MaxEntInstance](new java.io.File(diskCache+"/"+i.toString))
  }

  override def length = en - st
  override def splitAccessor(n: Int) : Seq[AccessSeq] = {
    throw new RuntimeException("splitAccessor unsupported with MaxEntDiskAccessSeq")
  }
}
