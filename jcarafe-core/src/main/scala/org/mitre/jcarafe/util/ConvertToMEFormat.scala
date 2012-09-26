package org.mitre.jcarafe.util

import collection.mutable.ArrayBuffer

/*
 * Utility object to convert feature vector data formatted for
 * aDNA project into jCarafe maxent format
 */
object ConvertToMEFormat {

  def mapLabel(l: String) : String = 
    if (l.length > 0) {
      if (l(0) == 'F') "Female"
      else "Male"
    } else "UNK"

  def mapLocLabel(l: String) : String = 
    if (l.length > 0) 
      l.replace(" ","")
    else "UNK"

  def main(args: Array[String]) = {
    val typ = args(0)
    val ostr = new java.io.FileOutputStream(new java.io.File(args(1)))
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    val lineRx = """^\[(.*)\]$""".r
    var lcnt = 0
    val srcs : Seq[(Int,Iterator[String])] = for (i <- 3 until args.length) yield (i,io.Source.fromFile(new java.io.File(args(i)))("UTF-8").getLines)
    val labSrc = io.Source.fromFile(new java.io.File(args(2)))("UTF-8").getLines
    println("typ = " + typ)
    val mapfn = if (typ == "LOC") mapLocLabel _ else mapLabel _
    while (labSrc.hasNext) {
      val lab = mapfn(labSrc.next)
      if (lab != "UNK") {
	os.write(lab)
	srcs foreach {case (i,s) =>
	val lineRx(vec) = s.next
        vec.split(',').foreach {el => 
	  if ((el != "") && (el != " ")) {
	    os.write(' ')
	    os.write(i.toString)
	    os.write('|')
	    os.write(el)
	  }}}
	os.write('\n')
      }
    }
    os.flush
    os.close
  }
}
