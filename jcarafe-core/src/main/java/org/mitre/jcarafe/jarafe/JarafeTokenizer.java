package org.mitre.jcarafe.jarafe;

/**
 * 
 * @author Ben Wellner
 */
public class JarafeTokenizer {

	public String tokenizeJsonString(String istr, boolean white) {
		org.mitre.jcarafe.tokenizer.FastTokenizer.jsonTokenizeString(istr, white);
	}
	
	public String tokenizeInlineString(String istr) {
		org.mitre.jcarafe.tokenizer.FastTokenizer.parseString(istr)
	}
}
