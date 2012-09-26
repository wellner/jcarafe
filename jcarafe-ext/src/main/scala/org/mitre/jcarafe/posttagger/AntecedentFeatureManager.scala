/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.posttagger
import org.mitre.jcarafe.crf._
import org.mitre.jcarafe.util._
import scala.collection.mutable.ListBuffer

abstract class AntecedentFeatureManager(iString: String, var maxNumStates: Int) extends NonFactoredFeatureManager[Array[PostTok]](iString) {
  
  
  def getAttribute(s: ObsSource[Array[PostTok]], att: String) : Option[String] = s.info match {case Some(s) => s.get(att) case None => None }
  
  def _distFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    new FeatureReturn("dist="+state.toString)
  }

/*  def _announceFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val u1 = getAttribute(src(cp),"user")
    u1 match {
     case Some("ANNOUNCEMENT") =>
      List(new NonFactoredPreFeature(false,-1,state,("ANNOUNCEMENT&&dist="+state.toString)))
     case Some(v) => 
      List(new NonFactoredPreFeature(false, -1, state, ("NonANNOUNCEMENT&&dist="+state.toString))) 
    }
  }*/
  
  def _sameFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val fn = if (state == 0) "-SAME-" else "-DIFF-"
    new FeatureReturn(fn)
  }
  
  def _sameUserFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val u1 = getAttribute(src(cp),"user")
    val u2 = getAttribute(src(cp-state),"user")
    val sameStr = if (u1 == u2) "sameUser" else "diffUser"
    new FeatureReturn(sameStr)
  }
  
  def _binnedHypLength(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
	val hypLength = src(cp - state).obs.length
	if(hypLength < 5){
	  new FeatureReturn("CandidateLength<5")
	} else if(hypLength < 10){
	  new FeatureReturn("5<CandidateLength<10")
	} else if(hypLength < 20){
	  new FeatureReturn("10<CandidateLength<20")
    } else if(hypLength < 35){
      new FeatureReturn("20<CandidateLength<35")
    } else if(hypLength < 50){
      new FeatureReturn("35<CandidateLength<50")
    } else{
      new FeatureReturn("50<CandidateLength")
    }  
  }
  
  def _binnedLength(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val hypLength = src(cp).obs.length
    if(hypLength < 5){
	  new FeatureReturn("CurrentLength<5")
	} else if(hypLength < 10){
	  new FeatureReturn("5<CurrentLength<10")
	} else if(hypLength < 20){
	  new FeatureReturn("10<CurrentLength<20")
    } else if(hypLength < 35){
      new FeatureReturn("20<CurrentLength<35")
    } else if(hypLength < 50){
      new FeatureReturn("35<CurrentLength<50")
    } else{
      new FeatureReturn("50<CurrentLength")
    }  
  }
  
  def _overlapFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val hypothesis = src(cp - state).obs
    val reference = src(cp).obs
    var overlap = 0
    var wordCount = 0
    hypothesis.toString.split(' ').foreach { word =>
    	if(word != "" && reference.toString.split(' ').contains(word))
       		overlap += 1
       	wordCount += 1
    }
    overlap = overlap / wordCount
    new FeatureReturn("Overlap="+overlap.toString)
  }
  
  def _lexOverlapFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val featureList = new ListBuffer[String]()
    val wordList = new ListBuffer[String]()
    val hypothesis = src(cp - state).obs
    val referenceWords = src(cp).obs.toString.split(' ')
    hypothesis.toString.split(' ').foreach { word =>
    	if(word != "" && referenceWords.contains(word) && !wordList.contains(word))
       		featureList += "overlap="+word
    		wordList += word
    }
    new FeatureReturn(featureList.toList)
  }
  
  def _refLexFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val reference = src(cp).obs
    val featureList = new ListBuffer[String]()
    reference.toString.split(' ').foreach { word =>
      	featureList += "refLex="+word
    }
    new FeatureReturn(featureList.toList)
  }
  
  def _hypLexFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val hypothesis = src(cp-state).obs
    val featureList = new ListBuffer[String]()
    hypothesis.toString.split(' ').foreach { word =>
      	featureList += "hypLex="+word
    }
    new FeatureReturn(featureList.toList)
  }
  
  def _overlap2Fn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val stopWordList = Set("a", "able", "about", "above", "according", "accordingly", "across", "actually", "after", "afterwards", "again", "against", "all", "allow", "allows", "almost", "alone", "along", "already", "also", "although", "always", "am", "among", "amongst", "an", "and", "another", "any", "anybody", "anyhow", "anyone", "anything", "anyway", "anyways", "anywhere", "apart", "appear", "appreciate", "appropriate", "are", "around", "as", "aside", "ask", "asking", "associated", "at", "available", "away", "awfully", "be", "became", "because", "become", "becomes", "becoming", "been", "before", "beforehand", "behind", "being", "believe", "below", "beside", "besides", "best", "better", "between", "beyond", "both", "brief", "but", "by", "came", "can", "cannot", "cant", "cause", "causes", "certain", "certainly", "changes", "clearly", "come", "comes", "concerning", "consequently", "consider", "considering", "contain", "containing", "contains", "corresponding", "could", "course", "currently", "definitely", "described", "despite", "did", "different", "do", "does", "doing", "done", "down", "downwards", "during", "each", "eg", "eight", "either", "else", "elsewhere", "enough", "entirely", "especially", "etc", "even", "ever", "every", "everybody", "everyone", "everything", "everywhere", "ex", "exactly", "example", "except", "far", "few", "fifth", "first", "five", "followed", "following", "follows", "for", "former", "formerly", "forth", "four", "from", "further", "furthermore", "get", "gets", "getting", "given", "gives", "go", "goes", "going", "gone", "got", "gotten", "greetings", "had", "happens", "hardly", "has", "have", "having", "he", "hello", "help", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "hi", "him", "himself", "his", "hither", "hopefully", "how", "howbeit", "however", "i", "ie", "if", "ignored", "immediate", "in", "inasmuch", "inc", "indeed", "indicate", "indicated", "indicates", "inner", "insofar", "instead", "into", "inward", "is", "it", "its", "itself", "just", "keep", "keeps", "kept", "know", "knows", "known", "last", "lately", "later", "latter", "latterly", "least", "less", "lest", "let", "like", "liked", "likely", "little", "look", "looking", "looks", "ltd", "mainly", "many", "may", "maybe", "me", "mean", "meanwhile", "merely", "might", "more", "moreover", "most", "mostly", "much", "must", "my", "myself", "name", "namely", "nd", "near", "nearly", "necessary", "need", "needs", "neither", "never", "nevertheless", "new", "next", "nine", "no", "nobody", "non", "none", "noone", "nor", "normally", "not", "nothing", "novel", "now", "nowhere", "obviously", "of", "off", "often", "oh", "ok", "okay", "old", "on", "once", "one", "ones", "only", "onto", "or", "other", "others", "otherwise", "ought", "our", "ours", "ourselves", "out", "outside", "over", "overall", "own", "particular", "particularly", "per", "perhaps", "placed", "please", "plus", "possible", "presumably", "probably", "provides", "que", "quite", "qv", "rather", "really", "reasonably", "regarding", "regardless", "regards", "relatively", "respectively", "right", "said", "same", "saw", "say", "saying", "says", "second", "secondly", "see", "seeing", "seem", "seemed", "seeming", "seems", "seen", "self", "selves", "sensible", "sent", "serious", "seriously", "seven", "several", "shall", "she", "should", "since", "six", "so", "some", "somebody", "somehow", "someone", "something", "sometime", "sometimes", "somewhat", "somewhere", "soon", "sorry", "specified", "specify", "specifying", "still", "sub", "such", "sup", "sure", "take", "taken", "tell", "tends", "th", "than", "thank", "thanks", "thanx", "that", "thats", "the", "their", "theirs", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "theres", "thereupon", "these", "they", "think", "third", "this", "thorough", "thoroughly", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "took", "toward", "towards", "tried", "tries", "truly", "try", "trying", "twice", "two", "under", "unfortunately", "unless", "unlikely", "until", "unto", "up", "upon", "us", "use", "used", "useful", "uses", "using", "usually", "uucp", "value", "various", "very", "via", "viz", "vs", "want", "wants", "was", "way", "we", "welcome", "well", "went", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "willing", "wish", "with", "within", "without", "wonder", "would", "would", "yes", "yet", "you", "your", "yours", "yourself", "yourselves", "zero", "he'd", "they'd", "i'm", "we'd", "you'd", "i'd", "a'", "all's", "an'", "an't", "and's", "another's", "as't", "at's", "b'", "be's", "be'st", "be't", "be'to", "by'", "by'r", "can't", "conn'd", "do't", "does't", "done't", "donn'd", "doo's", "e'en", "e'er", "ere't", "err'd", "for's", "for't", "from's", "from't", "go'st", "ha'", "ha't", "had'", "have't", "he'ld", "he'll", "he's", "here's", "hew'd", "him'", "him's", "him't", "how'er", "how's", "howe'er", "howl'd", "howl'st", "howso'er", "howsoe'er", "howsome'er", "i'", "if't", "in's", "in't", "into't", "is't", "it's", "let's", "let'st", "let't", "m'en", "may'st", "may't", "ne'er", "o'", "o'er", "of's", "off's", "on's", "on't", "one's", "other's", "others'", "shall's", "shall't", "she'd", "she'ld", "she'll", "she's", "shew'd", "so'", "so's", "soe'er", "sow'd", "sow't", "ta'en", "th'", "that's", "thaw'd", "thee't", "there's", "there't", "they'ld", "they'll", "they're", "they've", "this't", "thou'dst", "thou'ldst", "thou'lt", "thou'rt", "thou's", "thou'st", "thou't", "to's", "to't", "undo't", "up'", "upon's", "upon't", "us'", "we'ld", "we'll", "we're", "went'st", "were't", "what's", "whate'er", "whatsoe'er", "whene'er", "where'er", "where's", "where't", "whereso'er", "wheresoe'er", "whet'st", "who'll", "who's", "whoe'er", "whore's", "wi'", "will'd", "will't", "woo'd", "woo't", "wooer's", "ye'll", "ye're", "ye've", "yet'", "you'", "you'ld", "you'll", "you're", "yourself's") 
    val hypothesis = src(cp - state).obs
    val reference = src(cp).obs
    var overlap = 0
    var wordCount = 0
    hypothesis.toString.split(' ').foreach { word =>
        if(!stopWordList.contains(word)){
	        if(word != "" && reference.toString.split(' ').contains(word))
       		    overlap += 1
       	    wordCount += 1
        }
    }
    overlap = if(wordCount > 0) overlap / wordCount else -1        
    new FeatureReturn("Overlap="+overlap.toString)
  }
  
  
  def _previousLengthFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
	new FeatureReturn("CandidateLength="+src(cp - state).obs.length)
  }
  
/*  def _currentLengthFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
	List(new NonFactoredPreFeature(false,-1,state,"CurrentLength="+src(cp).obs.length))
  }*/
  
  def _lengthRatioFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
	val hypLength = src(cp - state).obs.length
    val refLength = src(cp).obs.length
    if(refLength == 0){
      new FeatureReturn("hypLen/refLen>3.00")
    }else{
    	val ratio = hypLength / refLength
    	if(ratio < 0.333){
	  new FeatureReturn("hypLen/refLen<0.333")
    	}else if(ratio < 0.666){
	  new FeatureReturn("0.333<hypLen/refLen<0.666")
    	}else if(ratio < 1.5){
	  new FeatureReturn("0.666<hypLen/refLen<1.50")
    	}else if(ratio < 3){
	  new FeatureReturn("1.50<hypLen/refLen<3.00")
    	}else{
	  new FeatureReturn("hypLen/refLen>3.00")
    	}
    }
  }
  
/*  def _addressedFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val reference = src(cp).obs.toString.toLowerCase
    val hypotheticalUser = getAttribute(src(cp - state),"user").get.toString.toLowerCase
    var addressed = "False"
    println("In addressed, user is " + hypotheticalUser)
    println("Reference is " + reference)
   	if(reference.contains(hypotheticalUser))
   		addressed = "True"
    List(new NonFactoredPreFeature(false,-1,state,"Addressed="+addressed))
  }*/
  
  def _endingCharsFn(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    if(src(cp-state).obs.length > 0){
      val endOfHypObs = src(cp-state).obs(src(cp-state).obs.length-1).toString
      if(endOfHypObs == "." || endOfHypObs == "!"){
	new FeatureReturn("HypEndingPunct")
      }else if (endOfHypObs == "?"){
	new FeatureReturn("HypEndingQuestionMark")
      }else{
	new FeatureReturn("HypNoInteresingPunct")
      }
    }else{
      new FeatureReturn("HypLenIsZero")
    }
   }

  def _isAnnounce(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val u1 = getAttribute(src(cp),"user")
    val fn = u1 match {
      case Some("ANNOUNCEMENT") => "isAnnounce"
      case _ => "noAnnounce" 
    }
    new FeatureReturn(fn)
  }
  
  def _prevIsAnnounce(ss:Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val u1 = getAttribute(src(cp-state),"user")
    val fn = u1 match {
      case Some("ANNOUNCEMENT") => "prevIsAnnounce"
      case _ => "prevIsNotAnnounce" 
    }
    new FeatureReturn(fn)
  }
  
  def _isAddressed(ss: Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
	getAttribute(src(cp-state),"user") match { 
	  case Some(u) =>
	    val du = u.toLowerCase
	    if (src(cp).obs exists {se => se.toString.toLowerCase.startsWith(du) }) new FeatureReturn("MENTION") else new FeatureReturn
	  case None => new FeatureReturn}
  }
  
  def _DAPair(ss: Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val da1 = getAttribute(src(cp),"dialog-act")
    val da2 = getAttribute(src(cp-state),"dialog-act")
    new FeatureReturn("DApair="+da1+da2)
  }
  
  def _length(ss: Int, src: SourceSequence[Array[PostTok]], cp: Int, state: Int) : FeatureReturn = {
    val l = src(cp).obs.length.toString
    new FeatureReturn("curLen="+l)
  }
 
  def basicFns(ss: Int, src: SourceSequence[Array[PostTok]], cp: Int) : FeatureReturn = {
    val obs = src(cp).obs
    if (obs.length > 0) {
      val curStr = obs(0)
      val backTo = cp min maxNumStates
      val fbuf = new scala.collection.mutable.ListBuffer[String]
      var i = 0
      while (i <= backTo) {
    	//fbuf += new NonFactoredPreFeature(false,-1,i,(preO(0) + curStr))
    	
    	val u1 = getAttribute(src(cp),"user")
    	val u2 = getAttribute(src(cp-i),"user")
    	u1 match {
    	  case Some("ANNOUNCEMENT") =>
    	    fbuf += "ANNOUNCEMENT&&dist="+i.toString
          case Some(v) => 
           fbuf += "NonANNOUNCEMENT&&dist="+i.toString
          case	_ => fbuf += ("dist="+i.toString)
    	}
    	if (i > 0 && (u1 == u2)) {
    	  fbuf += "sameUser&&dist="+i.toString
    	  //fbuf += new NonFactoredPreFeature(false,-1,i,("sameUser"))
    	}	
        i += 1
      }
      new FeatureReturn(fbuf.toList)
      }
    else new FeatureReturn
  }
  
  //val fnList = List(basicFns _)
  val fnList = Nil
  //val fineFnList = Nil
  
}
object AntecedentFeatureManager {
  
  import scala.io.Source
  
  def getFeatureSpecString(f:String) = {
    val sbuf = new StringBuffer
    Source.fromFile(new java.io.File(f)).getLines() foreach {sbuf.append(_)} 
    sbuf.toString
  }
  
  def getMgrDecode(opts:Options, model:NonFactoredModel) : DynamicAntecedentFeatureManager = 
    new DynamicAntecedentFeatureManager(model.numStates,model.fspec)
  
  def getMgrTrain(opts:Options) = new DynamicAntecedentFeatureManager(opts.numStates.get, getFeatureSpecString(opts.featureSpec.get)) 
  
  def apply(opts: Options) : AntecedentFeatureManager = 
    opts.featureSpec match {case Some(m) => getMgrTrain(opts) 
                            case None => throw new RuntimeException("Feature spec required as input to training") } 
                              
  
  def apply(opts: Options, m: NonFactoredModel) : AntecedentFeatureManager = getMgrDecode(opts,m)
}


