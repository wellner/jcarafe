name := "jcarafe"

version := "0.9.8.5.b-01"

scalaVersion in ThisBuild := "2.10.0"

organization := "org.mitre"

resolvers += Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.5.0"

libraryDependencies += "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.0"

libraryDependencies += "org.scala-tools.sbinary" % "sbinary_2.9.0" % "0.4.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"