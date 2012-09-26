package org.mitre.jcarafe.jarafe;

import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import scala.Tuple2;
import org.mitre.jcarafe.maxent.*;

public class JarafeMETrainer {
	
  RuntimeMaxEntTrainer maxent = null;
  
  /*
   * Creates a new Maximum Entropy trainer
   */
  public JarafeMETrainer() {
    maxent = new RuntimeMaxEntTrainer(10.0); // default Gaussian prior of 10.0
  }

  public JarafeMETrainer(double gp) {
    maxent = new RuntimeMaxEntTrainer(gp);
  }

  /*
   * @param label - A string value that is the "gold standard" label for this training instance
   * @param features - A list of strings that correspond to the names of the features present for this training instance
   */
  public void addTrainingInstance(String label, List<String> features) {
    maxent.addInstance(label,features);
  }

  /*
   * @param id - an integer id that can be used to re-assign this classification instance
   * @param label - A string value that is the "gold standard" label for this training instance
   * @param features - A list of strings double pairs that correspond to the names of the features present for this training instance
   */
  public void addValuedTrainingInstance(int id, String label, List<StringDoublePair> features) {
    List<scala.Tuple2<String,Double>> arr = new ArrayList<scala.Tuple2<String,Double>>();
    for (StringDoublePair pair : features) {
      arr.add(new scala.Tuple2<String,Double>(pair.getString(), pair.getDouble()));
    }
    maxent.addValuedInstance(new java.lang.Integer(id), label,arr);
  }

  /*
   * @param label - A string value that is the "gold standard" label for this training instance
   * @param features - A list of strings double pairs that correspond to the names of the features present for this training instance
   */
  public void addValuedTrainingInstance(String label, List<StringDoublePair> features) {
    List<scala.Tuple2<String,Double>> arr = new ArrayList<scala.Tuple2<String,Double>>();
    for (StringDoublePair pair : features) {
      arr.add(new scala.Tuple2<String,Double>(pair.getString(), pair.getDouble()));
    }
    maxent.addValuedInstance(label,arr);
  }
	
  /*
    @return model - A serialized string representation of the resulting trained model
   */
  public String train() {
    return maxent.batchTrain();
  }

	
	
}
