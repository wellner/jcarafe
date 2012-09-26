package org.mitre.jcarafe.jarafe;

import java.io.File;
import org.mitre.jcarafe.crf.DirectDecoder;
import java.util.List;
import java.util.ArrayList;
import scala.Tuple2;

/*
 * A very simple (and preliminary) API for CRF decoding when all features are computed
 * externally. A JCarafeDirectDecoder object should be created by passing an already trained
 * model file to the constructor. 
 */
public class JCarafeDirectDecoder {

	DirectDecoder decoder = null;
	
	public JCarafeDirectDecoder(File modelFile) {
		decoder = DirectDecoder.apply(modelFile);
	}
	
	/*
	 * This method takes an embedded list structure.  At the top it's a list of sequences. Each sequence consists
	 * of a sequence of feature lists, one feature list for each element/position in the sequence. Typically
	 * a single element/position would be an individual word/token for a phrase identification task. 
	 * 
	 * seqs = [Seq1, Seq2, ....., SeqN]
	 * Seq1 = [Position1, Position2, .... PositionM]
	 * Position1 = [Feature1, Feature2, .... FeatureK]
	 * 
	 * The return value is a list of lists of strings where each string element indicates the label or class 
	 * value assigned to each position within each sequence.
	 */
	public List<List<String>> decodeSequences(List<List<List<String>>> seqs) {
		return decoder.decodeFromJava(seqs);
	}
	
	
	/*
	 * This method works the same but takes in <i>SeqElement</i> objects instead of 
	 * lists features.
	 */
	public List<List<String>> decodeLabeledSequences(List<List<SeqElement>> seqs) {
		List<List<List<String>>> n1 = new ArrayList<List<List<String>>>();
		for (List<SeqElement> seq : seqs) {
			List<List<String>> n2 = new ArrayList<List<String>>();
			for (SeqElement el : seq) {
				n2.add(el.getFeatures());
			}
			n1.add(n2);
		}
		return decoder.decodeFromJava(n1);
	}
}
