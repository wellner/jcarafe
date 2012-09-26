package org.mitre.jcarafe.jarafe;

/*
  A label posterior pair contains a label and the posterior probability of that
  label according to the classifier - P(label|features)
*/

public class StringDoublePair {
    public Double v;
    public String s;

    public StringDoublePair(String s, double v) {
      this.v = v;
      this.s = s;
    }

    /*
      @return posterior - a double representing the posterior value
     */
    public Double getDouble() {
      return v;
    }

    /*
      @return label - a string representing the label
     */
    public String getString() {
      return s;
    }
  }
