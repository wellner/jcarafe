jcarafe
=======

====================================
Build Instructions
====================================


This README provides basic instructions for how to build jCarafe
from source.

0) Ensure that you have a JDK installed (i.e. "javac") and available on your working path.

1) Download and install the latest version of Simple Build Tool (SBT)

   Available at: http://www.scala-sbt.org

   ** Note: Ensure proxy settings are appropriate and ensure your machine has
      a connection to the Internet.  See the SBT documentation for setting
      the http proxy.  Typically, this involves adding 
      "-Dhttp.proxyHost=<hostname>" and "-Dhttp.proxyPort=<port number>" as 
      arguments to the "java" invocation that launches SBT.  
      It is also recommended to set the Java runtime heap allocation to 
      at least 1024m, more if available.
   

2) Place the "sbt" shell script or "sbt.bat" file in your path, following the SBT instructions.

3) Build jcarafe-core and jcarafe-ext.  Execute the commands below from the top-level "jcarafe" directory.

       cmd> sbt "project jcarafe-core" javacc "project jcarafe" assembly

This command will generate the jar files 

       jcarafe-core/target/jcarafe-core-assembly-0.9.8.x.jar
       jcarafe-ext/target/jcarafe-ext-assembly-0.9.8.x.jar

Step (3) above will produce a single .jar file with all the class files 
found in jcarafe-core and a separate .jar file in jcarafe-ext that includes all
of jcarafe-core and jcarafe-ext.  Note that other build targets are available
for packaging up jcarafe as a library (e.g. "sbt package").  See the SBT documentation
for more details.

=====================================
Generating Source-level Documentation
=====================================

Enter the top-level "jcarafe/" directory and execute the following SBT command:

  cmd> sbt doc

This will generate source-level documentation in the following directories:

        jcarafe/jcarafe-core/target/scala-2.10/api
        jcarafe/jcarafe-ext/target/scala-2.10/api
