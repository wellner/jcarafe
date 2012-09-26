jcarafe
=======

====================================
Build Instructions
====================================

This README provides basic instructions for how to build jCarafe
from source.

1) Download and install the latest version of Simple Build Tool (SBT)

   Available at: https://github.com/harrah/xsbt/wiki/Getting-Started-Setup

2) Place the "sbt" shell script or "sbt.bat" file in your path

3) Given the source distribution with top-level directory "jcarafe", enter this
   directory and do the following:

       a) Build jcarafe-core:
       cmd> sbt "project jcarafe-core" javacc proguard

       This command will build a compact executable jar file in:
       jcarafe-core/target/scala_2.9.1/jcarafe-core_2.9.1-0.9.8.x.min.jar

   [[ NOTE: There are an excessive number of warnings that appear right now for
      this proguard build. This is normal. ]]

   b) Build jcarafe-ext:
       cmd> sbt "project jcarafe-ext" assembly

       This command will be an "uber jar" file in:
       jcarafe-ext/target/jcarafe-ext-assembly-0.9.8.x.jar


(3a) above will produce a compact jar file appropriate for using core jCarafe
functionality, namely training and applying phrase extraction models.

(3b) generates a jar file that includes a number of extensions of jCarafe and 
additional utility algorithms.

=====================================
Generating Source-level Documentation
=====================================

Enter the top-level "jcarafe/" directory and execute the following SBT command:

  cmd> sbt doc

This will generate source-level documentation in the following directories:

  jcarafe/jcarafe-core/target/scala-2.9.1/api
  jcarafe/jcarafe-ext/target/scala-2.9.1/api

