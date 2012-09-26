package org.mitre.jcarafe.jarafe;

import java.util.ArrayList;
import java.util.List;

public class SeqElement {

	String label = null;
	List<String> features = null;
	
	public SeqElement(String l, List<String> fs) {
		label = l;
	    features = fs;
	}
	
	public SeqElement(List<String> fs) {
		label = "UNK";
		features = fs;
	}
	
	public SeqElement(String l) {
		label = l;
		features = new ArrayList<String>();
	}
	
	public void addFeature(String f) {
		if (features != null) 
			features.add(f);
	}
	
	public String getLabel() {
		return label;
	}
	
	public List<String> getFeatures() {
		return features;
	}
	
	public scala.Tuple2<String,List<String>>getAsScalaTuple() {
		return new scala.Tuple2<String,List<String>>(label,features);
	}
}
