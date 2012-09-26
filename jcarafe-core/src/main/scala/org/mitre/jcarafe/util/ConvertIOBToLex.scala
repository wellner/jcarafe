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

object ConvertIOBToLex {

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

  def writeTok(os: Writer, pos: String, obs: String) = {
    val apos = mapPos(pos)
    val aobs = mapObs(obs)
    os.write("<lex pos=\"")
    os.write(apos)
    os.write("\">")
    os.write(aobs)
    os.write("</lex>")
  }

  def getLineInfoStd(lineRx: Regex)(line: String) : (String,String,String) = {
    val lineRx(lab,pos,obs) = line
    (lab,pos,obs)
  }

  def getLineInfoMayo(lineRx: Regex)(line: String) : (String,String,String) = {
    val lineRx(obs,pos,lab) = line
    (lab,pos,obs)
  }
  
  def main (args: Array[String]) = {
    val src = io.Source.fromFile(new java.io.File(args(0)))("UTF-8")
    val ostr = new java.io.FileOutputStream(new java.io.File(args(1)))
    val std = ((args.length < 3) || (args(2) == "std"))
    val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
    var lines = src.getLines() 
    var inside : Option[String] = None
    val lineFn = if (std) getLineInfoStd(""" \d+[ ]+\d+[ ]+(\S+)[ ]+(\S+)[ ]+(\S+).*""".r) _ else getLineInfoMayo("""(\S+) (\S+) (\S+)""".r) _ 
    lines foreach { line =>
      if (!((line.length < 1) || (line(0) == '#'))) {
	val (lab,pos,obs) = lineFn(line)
	if (lab == "O") {
	  inside match {case Some(v) => os.write("</"+v+">") case None => }
	  writeTok(os,pos,obs)
	  inside = None
	}
	else {
	  val sp = lab.split('-')
	  if (sp(0) == "B") {
	    inside match {case Some(v) => os.write("</"+v+">") case None => }
	    os.write("<"+sp(1)+">")
	    writeTok(os,pos,obs)
	    inside = Some(sp(1))
	  } else {
	    writeTok(os,pos,obs)
	  }
	}
      } else if (line.length < 1) {
	os.write("\n\n")
      }
		 }
    os.flush
    os.close
    ostr.close
  }

}
