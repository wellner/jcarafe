package org.mitre.jcarafe.crf

import org.mitre.jcarafe.maxent.{ SparseStatelessMaxEnt, MEOptions, MaxEntTrainingSeqGen }
import org.scalatest.{ FlatSpec }
import org.mitre.jcarafe.util.Options

class JavaSerializationSpec extends FlatSpec {

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
        fail(swrite.toString())
    }

  }

  def deserialize[T]: T = {
    val istr = new java.io.FileInputStream("ss")
    val objIn = new java.io.ObjectInputStream(istr)
    objIn.readObject().asInstanceOf[T]
  }

  "Basic java serialization test" should "serialize and deserialize String" in {

    serialize("this is a string")
    try {
      val obj = deserialize[String]
      println("got object back: " + obj)
    } catch { case e: Throwable => e.printStackTrace() }
  }

  "Serialize for Feature Extraction" should "serialize and deserialize properly" in {
    val sgen = new TrainingSeqGen[String](new Options) with JsonSeqGen
    serialize(sgen)
    try {
      val obj = deserialize[TrainingSeqGen[String]]
      println("got training seq gen back")
    } catch { case e: Throwable => e.printStackTrace() }
  }

}

object RunSerializationTest extends JavaSerializationSpec {
    
  def main(args: Array[String]): Unit = {
    
    val cc = new StaticFeatureManagerBuilder[String]() {
      def buildFeatureFns = {
        List(FeatureFn(wdFn),
        FeatureFn(lexFn),
        FeatureFn(wdFn).ngram(None,(-1 to 0)),
        FeatureFn(lexFn).over((-1 to 1)),
        FeatureFn(wdFn).over((-2 to 2))
        )
      }
    }
    
    val bb = new StaticFeatureManagerBuilder[String]("DEFAULT",None,None,None,None,false)
    
    val oo = bb.getFeatureManager
    //val oo = new FeatureManager("",None, None, None, None, fns)
    //val oo = bb.getFeatureManager
    //val oo = bb.getFeatureManager
    //val sgen = new DirectTrainingSeqGen[String](new Options) with JsonSeqGen
    //val a = new Alphabet[String]()
    serialize(oo)
    try {
      //val obj = deserialize[Alphabet[String]]
      val obj = deserialize[FeatureManager[String]]
      println("got obj back: " + obj)
    } catch { case e: Throwable => e.printStackTrace() }
    
  }
}