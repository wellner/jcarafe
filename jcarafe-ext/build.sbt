import AssemblyKeys._ 

name := "jcarafe-ext"

libraryDependencies += "org.mitre" % "jcarafe-core_2.9.1" % "0.9.8.4.RC1"

libraryDependencies += "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test"

libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.2"

resolvers += "Scala-Tools Maven2 Snapshot Repository" at "http://scala-tools.org/repo-snapshots"

resolvers += "mitre-chatter-repository" at "http://rcf.mitre.org/project/chatter/repo"

seq(ProguardPlugin.proguardSettings :_*)

seq(assemblySettings: _*)

proguardOptions ++= Seq(
		keepMain("org.mitre.jcarafe.tagger.GenericTagger"),
		keepMain("org.mitre.jcarafe.dparser.ParserTask"),
		keepMain("org.mitre.jcarafe.dparser.ProjectiveParserTask"),
		keepMain("org.mitre.jcarafe.posttagger.SummaryTaggerMain"),
		keepMain("org.mitre.jcarafe.posttagger.DATaggerMain"),
		keepMain("org.mitre.jcarafe.posttagger.AntecedentMain"),
		keepMain("org.mitre.jcarafe.scopetagger.ScopeParserMain"),
		keepMain("org.mitre.jcarafe.scopetagger.BioScopeCorpusMain"),
		keepMain("org.mitre.jcarafe.clustering.BrownClustering")
		)

publishTo := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))

publishMavenStyle := true