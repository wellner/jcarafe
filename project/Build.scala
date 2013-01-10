import sbt._
import Keys._


object BuildSettings { 
  val buildOrganization = "org.mitre"
  val buildVersion = "0.9.8.4.RC21"
  val buildScalaVersion = "2.9.2"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object Resolvers { 
  val scalaSnapshots = "Scala-Tools Maven2 Snapshot Repository" at "http://scala-tools.org/repo-snapshots"  
  val scalanlpRepo   = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo/"
  val internalRepo   = Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo")
}

object Dependencies { 
  val jacksonCore     = "org.codehaus.jackson" % "jackson-core-asl" % "1.5.0"
  val jacksonMapper   = "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.0"
  val sbinary         = "org.scala-tools.sbinary" % "sbinary" % "0.4.1"
  val testing         = "org.scalatest" % "scalatest" % "1.6.1" % "test"
}

object JCarafeBuild extends Build {

  import BuildSettings._
  import Resolvers._
  //import Dependencies._

  lazy val root = Project(id = "jcarafe",
                            base = file(".")) aggregate(jcarafeCore, jcarafeExt)

  lazy val jcarafeCore = Project(id = "jcarafe-core",
				    base = file("jcarafe-core"), settings=projSettings)

  lazy val jcarafeExt = Project(id = "jcarafe-ext",
				   base = file("jcarafe-ext"), settings=buildSettings) dependsOn(jcarafeCore)

  val outDirTargetFiles = Set(
    "GenToker.java",
    "GenTokerConstants.java",
    "GenTokerTokenManager.java",
    "JsonToker.java",
    "JsonTokerConstants.java",
    "JsonTokerTokenManager.java",
    "WhiteSpaceToker.java",
    "WhiteSpaceTokerTokenManager.java",
    "WhiteSpaceToker2.java",
    "WhiteSpaceToker2TokenManager.java",
    "ParseException.java",
    "SimpleCharStream.java",
    "Token.java",
    "TokenMgrError.java"
    )

  val javaccClassPath = file("project/build/lib") ** "*.jar"
  lazy val runJavaCC = TaskKey[Seq[File]]("javacc")
  lazy val javaCCFiles = TaskKey[Seq[File]]("javacc-files")

  private def javaCCFilesTask = (baseDirectory) map { (base: File) =>
      val fp = file("jcarafe-core") / "src" / "main" / "javacc" / "org" / "mitre" / "jcarafe" / "lexer" ** "*.jj"
      fp.get
    }

  val myManaged = file("jcarafe-core") / "src" / "main" / "java" / "org" / "mitre" / "jcarafe" / "lexer"

  private def srcGeneratorTask = (javaCCFiles in Compile) map { inFiles =>
    IO.createDirectory(myManaged)
    val outDirTargets = outDirTargetFiles map { myManaged / _ }
    inFiles foreach {file => ("java -cp " + javaccClassPath + " javacc -OUTPUT_DIRECTORY="+myManaged.absolutePath+" "+file.toString) ! }
    outDirTargets.toSeq
   }

  def projSettings = buildSettings ++ Seq(
    javaCCFiles in Compile <<= javaCCFilesTask,
    runJavaCC in Compile <<= srcGeneratorTask
  )

}
