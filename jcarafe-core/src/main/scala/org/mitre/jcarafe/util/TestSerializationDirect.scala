package org.mitre.jcarafe.util

import org.mitre.jcarafe.crf.{ PsaLearner, AbstractInstance }
import org.mitre.jcarafe.maxent.{ MEOptions, SparseStatelessMaxEnt }

/*
 * This utility needs to be a separate application as Serialization has issues within
 * ScalaTest testing framework.
 */
object TestSerializationDirect {

  def serialize[T](obj: T) = {
    val fs = new java.io.FileOutputStream("ss")
    val objOut = new java.io.ObjectOutputStream(fs)
    try {
      objOut.writeObject(obj)
    } catch {
      case e: Throwable =>
        val swrite = new java.io.StringWriter
        swrite.write("Serialization failed ...\n")
        e.printStackTrace(new java.io.PrintWriter(swrite))
        System.err.println(swrite.toString())
    }

  }

  def deserialize[T]: T = {
    val istr = new java.io.FileInputStream("ss")
    val objIn = new java.io.ObjectInputStream(istr)
    objIn.readObject().asInstanceOf[T]
  }

  class MaxEntStatelessTest(nls: Int, nfs: Int, opts: MEOptions) extends SparseStatelessMaxEnt(nls, nfs, opts)
    with PsaLearner[AbstractInstance] with Serializable

  def main(args: Array[String]) = {
    val meStateless = new MaxEntStatelessTest(2, 10, new MEOptions)
    serialize(meStateless)
    println("initial params = " + meStateless.numParams)
    try {
      val obj = deserialize[MaxEntStatelessTest]
      println("Object deserialization completed...")
      println("num params = " + obj.numParams)
    } catch {
      case e: Throwable =>
        val swrite = new java.io.StringWriter
        swrite.write("Deserialization failed ...\n")
        e.printStackTrace(new java.io.PrintWriter(swrite))
        System.err.println(swrite.toString())
    }
    
  }
}