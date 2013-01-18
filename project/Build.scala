import sbt._
import Keys._


object JCarafeBuild extends Build {

  lazy val root = Project(id = "jcarafe",
                            base = file(".")) aggregate(jcarafeCore, jcarafeExt)

  lazy val jcarafeCore = Project(id = "jcarafe-core",
				    base = file("jcarafe-core"), settings=projSettings)

  lazy val jcarafeExt = Project(id = "jcarafe-ext",
				   base = file("jcarafe-ext")) dependsOn(jcarafeCore)

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

  def projSettings = Defaults.defaultSettings ++ Seq(
    javaCCFiles in Compile <<= javaCCFilesTask,
    runJavaCC in Compile <<= srcGeneratorTask
  )

}
