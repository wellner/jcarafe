package org.mitre.jcarafe.jarafe;

import java.util.List;
import java.util.Arrays;
import org.mitre.jcarafe.maxent.*;

public class JarafeXValidator {

  Evaluator evaluator = null;

  public JarafeXValidator() {
    evaluator = new Evaluator();
  }
  
    /*
   * @param label - A string value that is the "gold standard" label for this training instance
   * @param features - A list of strings that correspond to the names of the features present for this training instance
   */
  public void addTrainingInstance(String label, List<String> features) {
    evaluator.addJInstance(label,features);
  }
  
  /*
   * @param numFolds - Number of x-validation folds to use
   * @param file - output file for generated x-validation report
   */
  public void generateReport(int numFolds, java.io.File file) {
    evaluator.xValidateAndGenerateReport(numFolds, file);
  }
  
}