package org.mitre.jcarafe.jarafe;

/**
 * 
 * @author Ben Wellner
 */
public class JarafeTokenizer {

	public String tokenizeJsonString(String istr, boolean white) {
		return org.mitre.jcarafe.tokenizer.FastTokenizer.jsonTokenizeString(istr, white);
	}
	
}
