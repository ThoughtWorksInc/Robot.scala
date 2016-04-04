organization := "com.thoughtworks.robot"

name := "robot"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "com.thoughtworks.q" %% "q" % "1.0.0-SNAPSHOT"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "com.lihaoyi" %% "utest" % "0.4.3" % Test

testFrameworks += new TestFramework("utest.runner.Framework")