package org.mitre.jcarafe.jarafe;

import org.mitre.jcarafe.maxent.*;
import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;

public class JarafeMEDecoder {
	
  RuntimeMaxEntDecoder maxEnt = null;

  
  /*
   * @param model - A serialized string representation of a model (produced via training)
   */	
  public JarafeMEDecoder(String model) {
    maxEnt = RuntimeMaxEntDecoder.apply(model);
  }

  /*
   * @param model - A serialized model file
   */	
  public JarafeMEDecoder(java.io.File file) {
    maxEnt = RuntimeMaxEntDecoder.apply(file);
  }

  /*
   * @param features - A list of strings representing the features for a classification instance
   * @return label - A string representing the predicted label according to the decoder
   */	
  public String classifyInstance(List<String> features) {
    return maxEnt.decodeInstance(features);
  }

  /*
   * @param features - A list of string-double pairs representing the valued features for a classification instance
   * @return label - A string representing the predicted label according to the decoder
   */	
  public String classifyValuedInstance(List<StringDoublePair> features) {
    List<scala.Tuple2<String,Double>> nfs = new ArrayList<scala.Tuple2<String,Double>>();
    for (StringDoublePair el : features) {
      nfs.add(new scala.Tuple2<String,Double>(el.getString(), el.getDouble()));
    }
    return maxEnt.decodeValuedInstance(nfs);
  }

  /*
   * @param features - A list of strings representing the features for a classification instance
   * @return labelposteriorpair list - A list of pairs that include each label and a posterior probability mass
   */	
  public List<StringDoublePair> classifyInstanceDistribution(List<String> features) {
    List<scala.Tuple2<String,Double>> r = maxEnt.decodeInstanceAsDistribution(features);
    List<StringDoublePair> res = new ArrayList<StringDoublePair>();
    for (scala.Tuple2<String,Double> el : r) {
      res.add(new StringDoublePair(el._1, el._2));
    }
    return res;
  }
  /*
   * @param features - A list of string-double pairs representing the valued features for a classification instance
   * @return string-double pair list - A list of pairs that include each label and a posterior probability mass
   */	
  public List<StringDoublePair> classifyValuedInstanceDistribution(List<StringDoublePair> features) {
    List<scala.Tuple2<String,Double>> nfs = new ArrayList<scala.Tuple2<String,Double>>();
    for (StringDoublePair el : features) {
      nfs.add(new scala.Tuple2<String,Double>(el.getString(), el.getDouble()));
    }
    List<scala.Tuple2<String,Double>> r = maxEnt.decodeValuedInstanceAsDistribution(nfs);
    List<StringDoublePair> res = new ArrayList<StringDoublePair>();
    for (scala.Tuple2<String,Double> el : r) {
      res.add(new StringDoublePair(el._1, el._2));
    }
    return res;
  }

}
