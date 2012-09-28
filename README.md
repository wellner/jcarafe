jcarafe
=======

====================================
Build Instructions
====================================


This README provides basic instructions for how to build jCarafe
from source.

0) Ensure that you have a JDK installed (i.e. "javac") and available on your working path.

1) Download and install the latest version of Simple Build Tool (SBT)

   Available at: https://github.com/harrah/xsbt/wiki/Getting-Started-Setup

2) Place the "sbt" shell script or "sbt.bat" file in your path

3) Build jcarafe-core and jcarafe-ext.  Execute the commands below from the top-level "jcarafe" directory.

       cmd> sbt "project jcarafe-core" javacc "project jcarafe" assembly

This command will generate the "uber jar" files 

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

  jcarafe/jcarafe-core/target/scala-2.9.2/api
  jcarafe/jcarafe-ext/target/scala-2.9.2/api


=====================================
Using Proguard to Generate a Compact Executable
=====================================

