/*
 Copyright The MITRE Corporation 2010.   All rights reserved.
 */

package org.mitre.jcarafe.util
import util.matching.Regex
import java.io.Writer
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.BufferedOutputStream
import java.io.File
import scala.io.Source

object ConvertCoNLLFormat {

  def mapPos(p: String) = p match {
    case "COMMA" | "," | "$" | "." | "''" | "``" | ":" | "(" | ")" | "`" | "'" | "#" => "PUNCT"
    case a => a
  }

  def mapObs(obs: String) = obs match {
    case "COMMA" => ","
    case "``" => "\""
    case "''" => "\""
    case a => a
  }

  def writeTok(os: Writer, obs: String, sp: Boolean) = {
    if (sp) os.write(' ')
    os.write(obs)
  }

  def getLineInfoStd(lineRx: Regex)(line: String) : (String,String) = {
    val lineRx(obs,_,_,lab) = line
    (lab,obs)
  }
  
  def main (args: Array[String]) = {
    val src = io.Source.fromFile(new java.io.File(args(0)))("UTF-8")
    val ostr = new java.io.FileOutputStream(new java.io.File(args(1)))
    val std = ((args.length < 3) || (args(2) == "std"))
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    var lines = src.getLines() 
    var inside : Option[String] = None
    var current = ""
    val lineFn = getLineInfoStd("""(\S+)[ ]+(\S+)[ ]+(\S+)[ ]+(\S+).*""".r) _ 
    lines foreach { line =>
      if (!((line.length < 1) || (line(0) == '#'))) {
	    val (lab,obs) = lineFn(line)
	if (lab == "O") {
	  inside match {case Some(v) => os.write("</"+v+">") case None => }
	  writeTok(os,obs,true)
	  inside = None
	  current = ""
	}
	else {
	  val sp = lab.split('-')
	  if (sp(1) == current) 
	    writeTok(os,obs,true)
	  else if (sp(0) == "I") {
	    if (sp(1) != current) {
	      inside match {case Some(v) => os.write("</"+v+">") case None => }
	      os.write(" <"+sp(1)+">")
	      writeTok(os,obs,false)
	      inside = Some(sp(1))
	    } else {
	      writeTok(os,obs,true)
	    }
	  }
	  current = sp(1)
	}
      } else if (line.length < 1) {
	inside match {case Some(v) => os.write("</"+v+">") case None =>}
	inside = None
	os.write("\n\n")
	current = ""
      }
		 }
    os.flush
    os.close
    ostr.close
  }

}
