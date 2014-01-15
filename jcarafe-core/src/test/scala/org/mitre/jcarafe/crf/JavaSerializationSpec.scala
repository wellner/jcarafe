package org.mitre.jcarafe.crf

import org.mitre.jcarafe.maxent.{SparseStatelessMaxEnt, MEOptions, MaxEntTrainingSeqGen}
import org.scalatest.{FlatSpec}

class JavaSerializationSpec extends FlatSpec {
  
  def serialize[T](obj: T) = {
    val fs = new java.io.FileOutputStream("ss")
    val objOut = new java.io.ObjectOutputStream(fs)
    try {
      objOut.writeObject(obj)
    } catch {case e: Throwable =>
      val swrite = new java.io.StringWriter
      swrite.write("Serialization failed ...\n")
      e.printStackTrace(new java.io.PrintWriter(swrite))
      fail(swrite.toString())}

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
    
}