import AssemblyKeys._ 

name := "jcarafe-ext"

libraryDependencies += "org.mitre" % "jcarafe-core_2.9.2" % "0.9.8.4.RC9"

libraryDependencies += "org.scalatest" % "scalatest_2.9.2" % "1.8" % "test"

seq(assemblySettings: _*)

publishTo := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))

publishMavenStyle := true

// seq(ProguardPlugin.proguardSettings :_*)

// proguardOptions ++= Seq(
//		keepMain("org.mitre.jcarafe.tagger.GenericTagger"),
//		keepMain("org.mitre.jcarafe.dparser.ParserTask"),
//		keepMain("org.mitre.jcarafe.dparser.ProjectiveParserTask"),
//		keepMain("org.mitre.jcarafe.posttagger.SummaryTaggerMain"),
//		keepMain("org.mitre.jcarafe.posttagger.DATaggerMain"),
//		keepMain("org.mitre.jcarafe.posttagger.AntecedentMain"),
//		keepMain("org.mitre.jcarafe.scopetagger.ScopeParserMain"),
//		keepMain("org.mitre.jcarafe.scopetagger.BioScopeCorpusMain"),
//		keepMain("org.mitre.jcarafe.clustering.BrownClustering")
//		)

