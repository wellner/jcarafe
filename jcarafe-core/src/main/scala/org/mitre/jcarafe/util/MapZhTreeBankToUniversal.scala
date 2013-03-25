package org.mitre.jcarafe.util

import java.io.{File,Writer,PrintWriter,OutputStream}

class MapZhTreeBankToUniversal {
  
  def handleLine(line: String) = {
    val sbuf = new StringBuilder
    line.split(' ') foreach { tok =>
      tok.split('_').toList match {
        case wd :: pos :: Nil =>
          sbuf append "<lex pos=\""
          sbuf append pos
          sbuf append "\">"
          sbuf append wd
          sbuf append "</lex>"
        case t => sbuf append tok 
      }
    }
    sbuf.toString
  }
  
  def mapFile(iFile: File, oFile: File) = {
    val src = io.Source.fromFile(iFile)("UTF-8")
    val os = new PrintWriter(oFile,"UTF-8")
    src.getLines foreach {l => os.write(handleLine(l))}
    os.close
  }
}

object MapZhTreeBankToUniversal {
  
  def main(args: Array[String]) = {
    val mapper = new MapZhTreeBankToUniversal
    val idir = new File(args(0))
    val odir = new File(args(1))
    idir.listFiles foreach {f =>
      println("Processing file: " + f)
      val ofile = new File(odir.getPath + "/" + f.getName())
      mapper.mapFile(f,ofile)
      }
  }
}