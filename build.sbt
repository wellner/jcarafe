name := "jcarafe"

version in ThisBuild := "0.9.8.6.b-16"

scalaVersion in ThisBuild := "2.10.2"

organization := "org.mitre"

resolvers in ThisBuild += Resolver.url("Typesafe Release Repository",url("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns)

resolvers in ThisBuild += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

publishTo in ThisBuild := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo"))
