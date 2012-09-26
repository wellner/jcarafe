package org.mitre.jcarafe.jarafe;

import org.mitre.jcarafe.crf.DecoderPipeline;

/**
* JarafeTagger is the jCarafe Java API for phrase tagging. An instance of this class should be viewed as 
* a particular <i>decoder instance</i> - i.e., an object that will identify phrases
* in textual input according to ONE specific set of guidelines.  For example, an English
* part-of-speech tagger decoder would be a single Jarafe object while a German part-of-speech
* tagger, which is realized via a separately trained model, would be another Jarafe instance.
* <p>
* Two different input formats are supported: "json" and "inline" as described in the 
* jCarafe Users' Guide.  Below are examples of how
* to initialize decoders for these formats.  Note that a single decoder, once initialized, can
* ONLY use the format it was initialized to.  It is not currently possible to use the
* same decoder object to process different formats.
* <p>
* JSON:
* <pre>
* JarafeTagger jdecoder = new JarafeTagger();
* jdecoder.initializeAsJson("my-model-file"); // assign model file and specify JSON mode
* </pre>
* alternatively, JSON decoders are initialized by default using the constructor call:
* 
* JarafeTagger jdecoder = new JarafeTagger("my-model-file") // create decoder, assign model and specify JSON mode
*
* TEXT:
* <pre>
* JarafeTagger jdecoder = new JarafeTagger();
* jdecoder.initializeAsText("my-model-file"); // assign model file and specify Text mode
* </pre>
* 
* It is also possible to instantiate a JarafeTagger object with the full range of options
* available to jCarafe decoders as described in the jCarafe Users' Guide. This is done by
* passing in an array of strings that represents the command-line arguments to pass to the
* decoder. The example below instantiates a JarafeTagger, but indicates that it should
* not do its own pre-processing using the "--no-pre-proc" option.
* 
* <pre>
* JarafeTagger jdecoder = new JarafeTagger();
* jdecoder.initialize("--model","my-model-file","--no-pre-proc"); // specifiy options as they would appear on the command-line
* </pre>
*/
public class JarafeTagger extends Jarafe {

  /**
   * Create an uninitialized instance of a JarafeTagger object. initializeAsText or initializeAsJson
   * must be called on this object prior to using the decoder to process text.
   */
  public JarafeTagger() {}
	
  /**
   * @param modelFile
   * Create a JarafeTagger decoder that returns standoff annotations. Invoking this constructor has the same effect as:
   * <pre>JarafeTagger tagger = new JarafeTagger()
   * tagger.initializeAsJson(modelFile)</pre>
   */
  public JarafeTagger(String modelFile) {
    decoderPipeline = DecoderPipeline.getJsonDecoder(modelFile);
  }
	
  /**
   * @param r
   * 
   * This initializes a Jarafe decoder by reading a model file from the InputStream 'in' rather than from a file
   */
  public JarafeTagger(java.io.InputStream in) {
    decoderPipeline = DecoderPipeline.getJsonDecoder(in);
  }
	
	
  /**
   * Initialize the decoder to take raw strings and return standoff annotations
   * @param modelFile   A string denoting a file path to a model file	 
   */
  public void initializeAsJson(String modelFile) {
    decoderPipeline = DecoderPipeline.getJsonDecoder(modelFile);
  }

  /**
   * Initialize the decoder taking in standard jCarafe arguments. 
   * For example, passing in the arguments --mode inline --seq-boundary "s" --model "m1"
   * would initialize the model file, designate inline processing and force sequence splits
   * when encountering tags
   * @param modelFile   A string denoting a file path to a model file	 
   */
  public void initialize(String [] args) {
    decoderPipeline = DecoderPipeline.apply(args);
  }
  
  /**
   * Initialize the decoder to take raw strings and return standoff annotations
   * @param modelFile   An InputStream object to read in a model file	 
   */
  public void initializeAsJson(java.io.InputStream in) {
    decoderPipeline = DecoderPipeline.getJsonDecoder(in);
  }
	
  /**
   * Initialize the decoder to take a raw string and return inline XML annotations
   * @param modelFile   A string denoting a file path to a model file
   */
  public void initializeAsText(String modelFile) {
    decoderPipeline = DecoderPipeline.getTextDecoder(modelFile);
  }
	
  /**
   * Initialize the decoder to take a raw string and return inline XML annotations
   * @param modelFile   An InputStream object to read in a model file
   */
  public void initializeAsText(java.io.InputStream in) {
    decoderPipeline = DecoderPipeline.getTextDecoder(in);
  }
	
	
}
