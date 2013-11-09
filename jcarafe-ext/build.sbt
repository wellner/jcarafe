name := "jcarafe-ext"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.7.6"

libraryDependencies += "org.codehaus.jackson" % "jackson-mapper-asl" % "1.7.6"

libraryDependencies += "org.scala-tools.sbinary" % "sbinary_2.10" % "0.4.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger")

publishTo := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))
