name := "js-scala"

organization := "EPFL"

version := "0.2-SNAPSHOT"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots, 
    //needed for custom build of scala test
    "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
    )

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test",
    "EPFL" %% "lms" % "0.2",
    "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "container",
    "org.eclipse.jetty" % "jetty-websocket" % "8.0.1.v20110908",
    "org.eclipse.jetty" % "jetty-servlet" % "8.0.1.v20110908",
    "javax.servlet" % "servlet-api" % "2.5" % "provided->default")
    
//--- End of Dependencies

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// import web plugin keys
seq(webSettings :_*)

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"
