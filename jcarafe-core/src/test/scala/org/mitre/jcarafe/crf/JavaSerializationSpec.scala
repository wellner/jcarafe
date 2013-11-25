package org.mitre.jcarafe.crf

import org.scalatest.{FlatSpec}

class JavaSerializationSpec extends FlatSpec {
  
  def serialize[T](obj: T) = {
    val fs = new java.io.FileOutputStream("ss")
    val objOut = new java.io.ObjectOutputStream(fs)
    objOut.writeObject(obj)
  }
  
  def deserialize[T] : T = {
    val istr = new java.io.FileInputStream("ss")
    val objIn = new java.io.ObjectInputStream(istr)
    objIn.readObject().asInstanceOf[T]        
  }
  
  "Basic java serialization test" should "serialize and deserialize String" in {
      
      serialize("this is a string")
      try {
        val obj = deserialize[String]
        println("got object back: " + obj)
      } catch { case e: Throwable => e.printStackTrace()}
  }
  
  "MaxEnt classifier serialization test" should "serialize and deserialize MaxEnt trainer object" in {
    
  }

}