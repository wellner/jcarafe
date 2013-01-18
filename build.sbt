name := "jcarafe"

version in ThisBuild := "0.9.8.5.b-02"

scalaVersion in ThisBuild := "2.10.0"

organization := "org.mitre"

resolvers in ThisBuild += Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo")

resolvers in ThisBuild += Resolver.url("Typesafe Release Repository",url("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns)

resolvers in ThisBuild += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.5.0"

libraryDependencies += "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.0"

libraryDependencies += "org.scala-tools.sbinary" % "sbinary_2.10" % "0.4.1"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"