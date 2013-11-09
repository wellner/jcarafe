name := "jcarafe-core"

organization := "org.mitre"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.7.6"

libraryDependencies += "org.codehaus.jackson" % "jackson-mapper-asl" % "1.7.6"

libraryDependencies += "org.scala-tools.sbinary" % "sbinary_2.10" % "0.4.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "com.twitter" % "chill_2.10" % "0.3.4"

mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger")

// seq(ProguardPlugin.proguardSettings :_*)

// mainClass in proguard := Some("org.mitre.jcarafe.tagger.GenericTagger")

// proguardOptions ++= Seq(keepMain("org.mitre.jcarafe.tagger.GenericTagger"),
//		        keepMain("org.mitre.jcarafe.maxent.ME"),
//			keepMain("org.mitre.jcarafe.tokenizer.FastTokenizer"),
//			"-keepattributes Exceptions,InnerClasses,Signature,Deprecated,*Annotation*",
//			"-keepclassmembers enum * { public static **[] values(); public static ** valueOf(java.lang.String); }"
//			)
