package org.mitre.jcarafe.crf

import org.mitre.jcarafe.maxent.{SparseMaxEntStateless, MEOptions, MaxEntTrainingSeqGen}
import org.scalatest.{FlatSpec}

class JavaSerializationSpec extends FlatSpec {
  
  def serialize[T](obj: T) = {
    val fs = new java.io.FileOutputStream("ss")
    val objOut = new java.io.ObjectOutputStream(fs)
    try {
      objOut.writeObject(obj)
    } catch {case e: Throwable => fail("Serialization failed...\n" + e.getStackTrace())}
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
  
  class MaxEntStatelessTest(nls: Int, nfs: Int, opts: MEOptions) extends SparseMaxEntStateless(nls, nfs, opts)
  with PsaLearner[AbstractInstance]
  
  "MaxEnt classifier serialization test" should "serialize and deserialize MaxEnt trainer object" in {
    val opts = new MEOptions
    val sgen = new MaxEntTrainingSeqGen(opts)
    val meStateless = new MaxEntStatelessTest(sgen.getNumberOfStates, sgen.getNumberOfFeatures, opts)
    serialize(meStateless)
    try {
      val obj = deserialize[MaxEntStatelessTest]
    } catch {case e: Throwable => fail("Deserialization failed.. \n" + e.getStackTrace())}
  }

}