package org.mitre.jcarafe.jarafe;

import org.mitre.jcarafe.maxent.RunTimeMaxEntDocumentDecoder;
import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.ArrayList;

public class DocumentClassifier {
	
	RunTimeMaxEntDocumentDecoder decoder = null;
	
	public DocumentClassifier(String model) {
		decoder = RunTimeMaxEntDocumentDecoder.apply(new File(model));
	}
	
	public DocumentClassifier(File file) {
		decoder = RunTimeMaxEntDocumentDecoder.apply(file);
	}

	public DocumentClassifier(InputStream is) {
		decoder = RunTimeMaxEntDocumentDecoder.apply(is);
	}
	
	public DocumentClassifier(RunTimeMaxEntDocumentDecoder d) {
		decoder = d;
	}
	
	/*
	   * @param features - A string representing a text unit (i.e. document)
	   * @return label - A string representing the predicted label according to the decoder
	   */	
	public String classifyDocument(String doc) {
		return decoder.decodeDocument(doc);
	}
	
	/*
	   * @param features - A file containing a single text unit (document)
	   * @return label - A string representing the predicted label according to the decoder
	   */
	public String classifyDocument(File doc) {
		return decoder.decodeDocument(doc);
	}
	
	private List<StringDoublePair> convertResult(List<scala.Tuple2<String,Double>> r) {
		List<StringDoublePair> res = new ArrayList<StringDoublePair>();
	    for (scala.Tuple2<String,Double> el : r) {
	      res.add(new StringDoublePair(el._1, el._2));
	    }
	    return res;
	}
	
	/*
	 * @param doc - A string representing a text unit (i.e. document)
	 * @return posterior - A list of string/double pairs that represent categorical values with their associated probability mass
	 */
	public List<StringDoublePair> getDocumentPosterior(String doc) {
		return convertResult(decoder.getDocumentPosterior(doc));
	}
	
	/*
	 * @param doc - A file containing a single text unit (document)
	 * @return posterior - A list of string/double pairs that represent categorical values with their associated probability mass
	 */
	public List<StringDoublePair> getDocumentPosterior(File doc) {
		return convertResult(decoder.getDocumentPosterior(doc));
	}
	
	public void serializeModelToFile(java.io.File file) {
		decoder.serializeToFile(file);
	}
	
}
