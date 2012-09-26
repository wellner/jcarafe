package org.mitre.jcarafe.util

object FastLoops {
  
  def forIndex(upTo: Int)(f: Int => Any) : Unit = {
    var i = 0
    while (i < upTo) { f(i); i += 1}
  }
}
