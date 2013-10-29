name := "jcarafe"

version in ThisBuild := "0.9.8.6.b-21-jvm-1.6"
//version in ThisBuild := "0.9.8.6.b-21"

scalaVersion in ThisBuild := "2.10.2"

organization := "org.mitre"

// using "ThisBuild" will make this global, effectively, across all sub-projects
resolvers in ThisBuild += Resolver.url("Typesafe Release Repository",url("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns)

resolvers in ThisBuild += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

publishTo in ThisBuild := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))

javacOptions in ThisBuild ++= Seq("-source","1.6","-target","1.6")

//scalacOptions in ThisBuild ++= Seq("-target:jvm-1.7")