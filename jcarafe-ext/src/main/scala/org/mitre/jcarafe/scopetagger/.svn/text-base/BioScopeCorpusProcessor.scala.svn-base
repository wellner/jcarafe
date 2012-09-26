package org.mitre.jcarafe.scopetagger

import scala.xml._

class BioScopeCorpusProcessor(val dom: NodeSeq) {
	
	def filterOut(id: String, sent: List[Node]) : List[Node] = sent match {
			case Elem(a,t,atts,d,children @ _*) :: r =>
			  val nchildren = filterOut(id,children.toList)
			  if (t == "cue" || t == "xcope") { 
			    if ((t == "cue" && atts("ref") == id) || (t == "xcope" && atts("id") == id)) {
			      val nt = if (t == "cue") "lex" else t
			 	  new Elem(a,nt,atts,d,nchildren:_*) :: filterOut(id,r)
			    } else {
			 	  filterOut(id,nchildren) ::: filterOut(id,r)
			    }
			  } else {
			 	  new Elem(a,t,atts,d,nchildren:_*) :: filterOut(id,r)
			  }
			case a :: r => a :: filterOut(id,r)
			case Nil => Nil
		}

	def process(ofile: java.io.File, negOrUnc: Option[String]) = {
		val ostr = new java.io.FileOutputStream(ofile)
		val os = new java.io.OutputStreamWriter(new java.io.BufferedOutputStream(ostr), "UTF-8")
		val sents = dom \\ "sentence"
		sents foreach { s =>
			val cueIds_i = negOrUnc match {
				case None => (s \\ "cue")
				case Some("unc") =>  (s \\ "cue") filter {_.attributes("type").text == "speculation"}
				case Some("neg") =>  (s \\ "cue") filter {_.attributes("type").text == "negation"}
				case _ => throw new RuntimeException("Unexpected cue type")
			}
			val cueIds = cueIds_i  map {_.attributes("ref").text}
			cueIds foreach {cue => 
			  val nsent = filterOut(cue,List(s))
			  nsent foreach {ns => os.write(ns.toString); os.write("\n\n")} 
			}
		}
		os.close
	}
}


object BioScopeCorpusMain {
	
	def main(argv: Array[String]) = {
		val dir = new java.io.File(argv(0))
		val odir = argv(1) 
		val negOrUnc = if (argv.length > 2) { if (argv(2) == "neg") Some("neg") else if (argv(2) == "unc") Some("unc") else None } else None
		dir.listFiles foreach {f =>
		 val x = XML.load(f.getPath)
		 val processor = new BioScopeCorpusProcessor(x)
		 val ofile = odir + "/" + f.getName
		 processor.process(new java.io.File(ofile), negOrUnc)}
	}
}
