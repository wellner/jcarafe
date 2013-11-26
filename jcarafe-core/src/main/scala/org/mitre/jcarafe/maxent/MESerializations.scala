/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.crf.CompactFeature

object MESerializations {
  import com.esotericsoftware.kryo.io.{Input => KInput, Output => KOutput}
  import com.twitter.chill.{EmptyScalaKryoInstantiator, AllScalaRegistrar}

  val instantiator = new EmptyScalaKryoInstantiator
  val kryo = instantiator.newKryo
  new AllScalaRegistrar()(kryo)
  kryo.register(classOf[MaxEntInstance])
  
  def writeInstance(i: MaxEntInstance, f: java.io.File) = {
    val os = new java.io.BufferedOutputStream(new java.io.FileOutputStream(f))
    val output = new KOutput(os)
    kryo.writeObject(output, i)
    os.close()
    output.close
  }
  
  def readInstance(kInput: KInput) : MaxEntInstance = {
    val m = kryo.readObject(kInput, classOf[MaxEntInstance])
    kInput.close()
    m
  }
  
  def readInstance(f: java.io.File): MaxEntInstance = {
    val is = new java.io.BufferedInputStream(new java.io.FileInputStream(f))
    val m = readInstance(is)
    is.close()
    m
  }

  def readInstance(is: java.io.InputStream): MaxEntInstance = {
    val kInput = new KInput(is)
    readInstance(kInput)
  }
  
  def readInstance(ba: Array[Byte]): MaxEntInstance = readInstance(new KInput(ba))
  
}
