package org.mitre.jcarafe.crf

import org.scalatest.{FlatSpec}

class JavaSerializationSpec extends FlatSpec {
  
  "Basic java serialization test" should "serialize and deserialize String" in {
      val fs = new java.io.FileOutputStream("ss")
      val objOut = new java.io.ObjectOutputStream(fs)
      objOut.writeObject("this is a string")
      
      try {
        val istr = new java.io.FileInputStream("ss")
        val objIn = new java.io.ObjectInputStream(istr)
        val obj = objIn.readObject().asInstanceOf[String]
        println("got object back: " + obj)
      } catch { case e: Throwable => e.printStackTrace()}
  }

}