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

  def sharedSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.mitre",
    version := "0.9.8.6.b-28",
    scalaVersion := "2.10.2",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/snapshots/",
    resolvers += Resolver.url("Typesafe Release Repository",url("http://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns),
    publishTo := Some(Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo")),
    javacOptions ++= Seq("-source","1.7","-target","1.7")
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
      "org.codehaus.jackson" % "jackson-core-asl" % "1.7.6",
      "org.codehaus.jackson" % "jackson-mapper-asl" % "1.7.6",
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
      "com.twitter" % "chill_2.10" % "0.3.5"
    )
  )

  def extSettings = sharedSettings ++ Seq(
    name := "jcarafe-ext",
    mainClass in Compile := Some("org.mitre.jcarafe.tagger.GenericTagger"),
    libraryDependencies ++= Seq(
      "org.codehaus.jackson" % "jackson-core-asl" % "1.7.6",
      "org.codehaus.jackson" % "jackson-mapper-asl" % "1.7.6",
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
      "com.twitter" % "chill_2.10" % "0.3.5"
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
