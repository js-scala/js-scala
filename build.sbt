name := "js-scala"

organization := "EPFL"

version := "0.3-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

//--- Dependencies

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.10.0"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.0"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.0" // for ScalaTest

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
    "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT")

//--- End of Dependencies

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"
