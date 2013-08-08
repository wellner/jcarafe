package org.mitre.jcarafe.jarafe;

/**
 * 
 * @author Ben Wellner
 */
public class JarafeTokenizer {

	public String whiteSpaceTokenizeJsonString(String istr) {
		return org.mitre.jcarafe.tokenizer.FastTokenizer.jsonTokenizeString(istr, true);
	}

	public String standardTokenizeJsonString(String istr) {
		return org.mitre.jcarafe.tokenizer.FastTokenizer.jsonTokenizeString(istr, false);
	}

}
