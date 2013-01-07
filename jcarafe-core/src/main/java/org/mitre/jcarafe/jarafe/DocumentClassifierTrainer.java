package org.mitre.jcarafe.jarafe;

import org.mitre.jcarafe.maxent.RunTimeMaxEntDocumentTrainer;

public class DocumentClassifierTrainer {
	
	public RunTimeMaxEntDocumentTrainer trainer = null;
	
	public DocumentClassifierTrainer() {
		trainer = new RunTimeMaxEntDocumentTrainer();
	}
	
	public DocumentClassifier trainClassifier() {
		return new DocumentClassifier(trainer.trainModelToDecoder());
	}
	
	public void addInstance(String document, String label) {
		trainer.addDocumentAsTrainingInstance(document, label);
	}
	
	public void addInstance(java.io.File file, String label) {
		if (label.length() > 35) {
			throw new RuntimeException("Label has suspiciously long string length: " + label.length());
		}
		trainer.addDocumentFileAsTrainingInstance(file, label);
	}
	
	public void addInstances(java.io.File dir, String label) {
		if (label.length() > 35) {
			throw new RuntimeException("Label has suspiciously long string length: " + label.length());
		}
		for (java.io.File file : dir.listFiles()) {
		  if (file.isFile()) trainer.addDocumentFileAsTrainingInstance(file, label);
		}
	}
	
	public double evaluateUsingCrossValidation() {
		int numInstances = trainer.getNumberOfInstances(); 
		if (numInstances > 20)
			return trainer.xvalidate();
		else
			throw new RuntimeException("Meaningful cross validation requires at least 20 training instances.  Current number = " + numInstances);
	}
	
}
