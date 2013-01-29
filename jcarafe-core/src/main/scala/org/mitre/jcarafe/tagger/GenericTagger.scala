/*
 Copyright The MITRE Corporation 2009-2010.   All rights reserved.
 */

package org.mitre.jcarafe.tagger
import org.mitre.jcarafe.crf.FeatureManager
import org.mitre.jcarafe.crf.Model

/**
 * Entry point to a generic tagger application.  This application invokes both trainers
 * and decoders that can each operate on <i>two</i> different input formats: JSON
 * standoff or as plain TEXT with inline tags.  
 * @author Ben Wellner
 */
object GenericTagger {
  def main(argv: Array[String]) = 
    if (argv.length < 1) TaggerTask.printUsage
    else {
      TaggerTask(argv).process()
      System.exit(0)
    }
}

