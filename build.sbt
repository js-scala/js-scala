name := "js-scala"

organization := "EPFL"

version := "0.2"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots, 
    //needed for custom build of scala test
    "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
    )

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test",
    "EPFL" %% "lms" % "0.2")
    
//--- End of Dependencies

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"
