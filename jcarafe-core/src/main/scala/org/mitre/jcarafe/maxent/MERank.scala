/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

object MERank {
  def main(argv: Array[String]) = {
    val me = new NonFactoredMaxEntClassifier(argv)
    me.process()
  }
}
