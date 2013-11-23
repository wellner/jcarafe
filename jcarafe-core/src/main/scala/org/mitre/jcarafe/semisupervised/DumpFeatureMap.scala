/*
 Copyright The MITRE Corporation 2011.   All rights reserved.
 */

package org.mitre.jcarafe.semisupervised

import collection.mutable.HashMap

object DumpFeatureMap {

  import org.mitre.jcarafe.crf.InducedFeatureMapProtocol

  def main(args: Array[String]) = {
    assert(args.length == 2)
    val ifile = new java.io.File(args(0))
    val ofile = new java.io.File(args(1))
    val mp = InducedFeatureMapProtocol.readFMap(ifile)
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(new java.io.FileOutputStream(ofile)), "UTF-8")
    mp foreach {case (l,ar) =>
      os.write(l + " => ")
      ar foreach {a => os.write(a.toString); os.write(' ')}
      os.write('\n')}
    os.flush
    os.close
  }
}
