import sbt._
import Keys._
import sbtassembly.Plugin._
import sbtassembly.AssemblyUtils._
import AssemblyKeys._

object JCarafeBuild extends Build {

  lazy val root = Project(id = "jcarafe",
                            base = file(".")).settings(rootSettings:_*) aggregate(jcarafeCore, jcarafeExt)

  lazy val jcarafeCore = Project(id = "jcarafe-core", 
				 base = file("jcarafe-core")).
                         settings(coreSettings: _*).
                         settings(projAssemblySettings: _*)                         

  lazy val jcarafeExt = Project(id = "jcarafe-ext", 
				 base = file("jcarafe-ext")).
                         settings(extSettings: _*).
                         settings(projAssemblySettings: _*).
                         settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*) dependsOn(jcarafeCore)

  val keyFile = new java.io.File("~/.ssh/id_rsa.pub")

  def sharedSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.mitre.jcarafe",
    version := "0.9.99",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.10.5","2.11.7"),
    publishTo := Some(       
       if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging
    ),
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://github.com/wellner/jcarafe.git")),
    pomExtra in Global := {
      <scm>
        <connection>scm:git:github.com:wellner/jcarafe.git</connection>
        <url>git@github.com/wellner/jcarafe.git</url>
      </scm>
      <developers>
        <developer>
          <id>wellner</id>
          <name>Ben Wellner</name>
          <url>https://github.com/wellner/jcarafe.git</url>
        </developer>
      </developers>
    },    
    scalacOptions += "-target:jvm-1.8",
    javacOptions ++= Seq("-source","1.8","-target","1.8")
  )

  def rootSettings =  sharedSettings ++ Seq(
    name := "jcarafe"
  )

  def coreSettings = sharedSettings ++ Seq(
    name := "jcarafe-core",
    mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger"),
    javaCCFiles in Compile <<= javaCCFilesTask,
    runJavaCC in Compile <<= srcGeneratorTask,
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.3.0",
      "junit" % "junit" % "4.12" % "test",
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "com.twitter" % "chill_2.11" % "0.8.0"
    )
  )

  def extSettings = sharedSettings ++ Seq(
    name := "jcarafe-ext",
    mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger"),
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.3.0",
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
      "com.twitter" % "chill_2.11" % "0.8.0"
    )
  )

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

  def projAssemblySettings = assemblySettings ++ Seq(
    test in assembly := {},
    mergeStrategy in assembly := conflictRobustMergeStrategy
  )

  val conflictRobustMergeStrategy: String => MergeStrategy = { 
    case "reference.conf" | "rootdoc.txt" =>
      MergeStrategy.concat
    case PathList(ps @ _*) if isReadme(ps.last) || isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
        case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
          MergeStrategy.discard
        case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.deduplicate
      }
    case _ => MergeStrategy.first
  }

  private val ReadMe = """(readme)([.]\w+)?$""".r  
  private def isReadme(fileName: String): Boolean =    
    fileName.toLowerCase match { 
      case ReadMe(_, ext) if ext != ".class" => true      
      case _ => false    }

  private val LicenseFile = """(license|licence|notice|copying)([.]\w+)?$""".r  
  private def isLicenseFile(fileName: String): Boolean =    
    fileName.toLowerCase match {      
      case LicenseFile(_, ext) if ext != ".class" => true // DISLIKE      
      case _ => false    }

}
