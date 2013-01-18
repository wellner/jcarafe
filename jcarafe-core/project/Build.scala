import sbt._
import Keys._
import java.io.File


object JCarafe extends Build {

  //import BuildSettings._
  //import Resolvers._
  //import Dependencies._

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

  //val javaccConfig = config("javacc")
  
  private def javaCCFilesTask = (baseDirectory) map { (base: File) =>
      val fp = file("src") / "main" / "javacc" / "org" / "mitre" / "jcarafe" / "lexer" ** "*.jj"
      fp.get
    }

  val myManaged = file("src") / "main" / "java" / "org" / "mitre" / "jcarafe" / "lexer"

  private def srcGeneratorTask = (javaCCFiles in Compile) map { inFiles =>
    val outDirTargets = outDirTargetFiles map { myManaged / _ }
    inFiles foreach {file => ("java -cp " + javaccClassPath + " javacc -OUTPUT_DIRECTORY="+myManaged.absolutePath+" "+file.toString) ! }
    outDirTargets.toSeq
    /*							       
    val cachedFun = FileFunction.cached(cache / "javaccGenSources", inStyle=FilesInfo.lastModified,outStyle=FilesInfo.exists) { (in: Set[File]) =>
      inFiles foreach {file => ("java -cp " + javaccClassPath + " javacc -OUTPUT_DIRECTORY="+dir.absolutePath+" "+file.toString) ! }
      outDirTargets }
    cachedFun(inFiles.toSet).toSeq
    */
   }

  def projSettings = Defaults.defaultSettings ++ Seq(
    javaCCFiles in Compile <<= javaCCFilesTask,
    runJavaCC in Compile <<= srcGeneratorTask
    //sourceGenerators in Compile <+= runJavaCC in Compile
  )

  lazy val root = Project("root", file("."), settings = projSettings)

  //lazy val publishTo = Resolver.sftp("Chatter Maven Repo", "beijing.mitre.org", "/afs/rcf/project/chatter/repo")

  //lazy val mavenLocal = Resolver.file("Local Repository", new java.io.File(Path.userHome+"/.m2/repository"))
  
}
