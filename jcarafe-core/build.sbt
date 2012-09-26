name := "jcarafe-core"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.5.0"

libraryDependencies += "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.0"

libraryDependencies += "org.scala-tools.sbinary" % "sbinary_2.9.0" % "0.4.0"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

libraryDependencies += "commons-lang" % "commons-lang" % "2.6" 

seq(ProguardPlugin.proguardSettings :_*)

seq(assemblySettings: _*)

proguardOptions ++= Seq(keepMain("org.mitre.jcarafe.tagger.GenericTagger"),
		        keepMain("org.mitre.jcarafe.maxent.ME"),
			keepMain("org.mitre.jcarafe.tokenizer.FastTokenizer"),
			"-keepattributes Exceptions,InnerClasses,Signature,Deprecated,*Annotation*",
			"-keepclassmembers enum * { public static **[] values(); public static ** valueOf(java.lang.String); }"
			)

mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger")

mainClass in proguard := Some("org.mitre.jcarafe.tagger.GenericTagger")

publishTo := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))

publishMavenStyle := true
