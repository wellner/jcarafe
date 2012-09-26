package org.mitre.jcarafe.jarafe;

//import org.apache.commons.codec.binary.Base64;
import org.mitre.jcarafe.crf.Decoder;
import org.mitre.jcarafe.crf.DecoderPipeline;
import org.mitre.jcarafe.util.Annotation;
import java.util.List;
import java.util.ArrayList;
import javax.xml.bind.DatatypeConverter;


/**
 * 
 * @author Ben Wellner
 */
public class Jarafe {
	
	Decoder decoder = null;
	DecoderPipeline decoderPipeline = null;	
	
	
	//Base64 codec = new Base64();
	
	/**
	 * Simple annotation object useful for simple interaction with the Carafe decoder
	 */
	public class LightAnnot {
		private int start;
		private int end;
		private String type;
		LightAnnot(int st, int en, String t) {
			start = st;
			end = en;
			type = t;
		}
		public int getStart() { return start; }
		public int getEnd() { return end; }
		public String getType() { return type; }
	}

	/**
	 * Process a text string as input and return a string representation of the annotations.
	 * The representation returned depends on what type of decoder the Jarafe instance
	 * was initialized with.
	 * @param s  Input string
	 * @return a string encoding  serialized object with added annotations 
	 */
	public String processString(String s) {
		if (decoderPipeline != null) {
			return decoderPipeline.processString(s);
		} else { throw new java.lang.RuntimeException("Decoder Not Initialized"); }
	}
	
	
	public String processBase64String(String encodedStr) {
		String decodedString = new String(DatatypeConverter.parseBase64Binary(encodedStr));
		return DatatypeConverter.printBase64Binary(processString(decodedString).getBytes());
	}

  public List<String> processStringList(List<String> strList) {
    return decoderPipeline.processStringList(strList);
  }

	/**
	 * Process a text string as input and return a List of LightAnnot objects that serve as
	 * stand-off annotations over the input text.
	 * @param s Input string
	 * @return a list of LightAnnot objects
	 */
	public List<LightAnnot> processRawStringAsAnnotList(String s) {
		Annotation[] annots = decoderPipeline.processRawStringAsAnnotList(s);
		List<LightAnnot> alist = new ArrayList<LightAnnot>();
		int i = 0;
		for (i = 0; i < annots.length; i++) { 
			Annotation ann = annots[i];
			alist.add(new LightAnnot(ann.st(),ann.en(),ann.typ().toString()));
		}
		return (alist);
	}
		
}
