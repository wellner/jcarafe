package org.mitre.jcarafe.util

import java.io.{File,Writer,PrintWriter,OutputStream}

class MapZhTreeBankToUniversal(val posMapping: Map[String,String]) {
  
  def handleLine(line: String) = {
    val sbuf = new StringBuilder
    line.split(' ') foreach { tok =>
      tok.split('_').toList match {
        case wd :: pos :: Nil =>
          sbuf append "<lex pos=\""
          sbuf append posMapping.get(pos).getOrElse("NOUN")
          sbuf append "\">"
          sbuf append wd
          sbuf append "</lex>"
        case t => sbuf append tok 
      }

      sbuf append ' '
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
  
  def loadMapping(f: File) = {
    var mp = Map[String,String]()
    io.Source.fromFile(f).getLines foreach { l =>
      l.split('\t').toList match {
        case o :: m :: Nil => mp += (o -> m)
        case _ =>
      }
    }
    mp
  }
  
  def main(args: Array[String]) = {
    if (args.length < 3) throw new RuntimeException("Insufficient number of arguments")
    val idir = new File(args(0))
    val odir = new File(args(1))
    val mappingFile = new File(args(2))
    val mapper = new MapZhTreeBankToUniversal(loadMapping(mappingFile))

    idir.listFiles foreach {f =>
      println("Processing file: " + f)
      val ofile = new File(odir.getPath + "/" + f.getName())
      mapper.mapFile(f,ofile)
      }
  }
}