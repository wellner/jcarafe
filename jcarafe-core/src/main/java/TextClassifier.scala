/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */
package org.mitre.jcarafe.maxent

import org.mitre.jcarafe.util._
import org.mitre.jcarafe.crf.TrainingSeqGen
import org.mitre.jcarafe.crf.TextSeqGen
import org.mitre.jcarafe.crf.InstanceSequence
import org.mitre.jcarafe.crf.MaxEntSerializer
import org.mitre.jcarafe.util.Options

class TextOptions(fsPath: String) extends Options() {
  featureSpec = Some(fsPath)
}

class TextClassifier(fsPath: String) {

  import MaxEntSerializer._

  val meTrainer = new RuntimeMaxEntTrainer(new TextOptions(fsPath))
  private var meDecoder : Option[RuntimeMaxEntDecoder] = None 

  private val sGen = new TrainingSeqGen[String](new TextOptions(fsPath)) with TextSeqGen
  
  private def toFeatureVector(str: String) : Set[String] = 
    gatherFeatures(sGen.createSeqsWithInput(sGen.deserializeFromString(str)))

  private def fileToFeatureVector(file: java.io.File) : Set[String] = 
    gatherFeatures(sGen.createSeqsWithInput(sGen.deserializeFromFile(file)))

  private def fileToFeatureVector(file: String) : Set[String] = 
    gatherFeatures(sGen.createSeqsWithInput(sGen.deserializeFromFile(file)))

  private def gatherFeatures(seqs: Seq[InstanceSequence]) = 
    seqs.foldLeft(Set():Set[String]){(cs,seq) => seq.iseq.foldLeft(cs){(cs1,se) => se.userVec.foldLeft(cs1){_ + _.getName}}}


  /*
   * Add training a labeled training instance (for subsequent training)
   * @param label - a string label
   * @param str   - the text document contents as a string
   */
  def addStringAsTrainingInstance(label: String, str: String) : Unit = {
    val features = toFeatureVector(str)
    meTrainer.addInstance(label, features)
  }

  def addFileAsTrainingInstance(label: String, file: String) : Unit = addFileAsTrainingInstance(label, new java.io.File(file))
  def addFileAsTrainingInstance(label: String, file: java.io.File) : Unit = {
    val features = fileToFeatureVector(file)
    meTrainer.addInstance(label, features)
  }

  private def classifyInstance(fs: Set[String]) = meDecoder match {
    case Some(meDecoder) => meDecoder.decodeInstance(fs)
    case None => throw new RuntimeException("Decoder not available") 
  }

  /*
   * Classify a document residing in a file
   * @param file     Input file
   * @return         The model's classification of the input document as a string
   */
  def classifyFile(file: java.io.File) : String = classifyInstance(fileToFeatureVector(file))

  /*
   * Classify a document residing in a file
   * @param file     Input file path (as a string)
   * @return         The model's classification of the input document as a string
   */
  def classifyFile(file: String) : String = classifyInstance(fileToFeatureVector(file))

  /*
   * Classify a document
   * @param file     Input document's contents as a string
   * @return         The model's classification of the input document as a string
   */
  def classifyDocument(str: String) : String = classifyInstance(toFeatureVector(str))

  /*
   * Classify a document and produce the posterior distribution over classes/labels
   * @param file     Input document's contents as a string
   * @return         The posterior distribution of the instance according to the model
   */
  def classifyDocumentAsDistribution(str: String) : java.util.List[(String,java.lang.Double)] = meDecoder match {
    case Some(meDecoder) => meDecoder.decodeInstanceAsDistribution(toFeatureVector(str))
    case None => throw new RuntimeException("Decoder not available") 
  }

  /*
   * Train a model with classification instances added to this object
   * @return  A decoder object that can be used to classify new instances
   */
  def train = {
    meDecoder = Some(RuntimeMaxEntDecoder(meTrainer.batchTrain))
  }


  /*
   * Train a model with classification instances added to this object
   * @param   A file path for where to store the serialized model on the filesystem
   */
  def trainToFile(file: java.io.File) : Unit = {
    val m = meTrainer.batchTrainToModel
    writeModel(m,file)
  }

  /*
   * Train a model with classification instances added to this object
   * @param   A file path as a string for where to store the serialized model on the filesystem
   */
  def trainToFile(f: String) : Unit = trainToFile(new java.io.File(f))

  /*
   * Initialize the classifier (for decoding) with the specified model file
   * @param  A file path for where to retrieve a serialized model file
   */
  def initializeClassifier(modelFile: java.io.File) : Unit = { meDecoder = Some(RuntimeMaxEntDecoder(modelFile)) }

  /*
   * Initialize the classifier (for decoding) with the specified model file
   * @param  A file path (as a string) for where to retrieve a serialized model file
   */
  def initializeClassifier(m: String) : Unit = initializeClassifier(new java.io.File(m))

  
}

