name := "lms-sandbox"

organization := "EPFL"

version := "0.1"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots, 
    //needed for custom build of scala test
    "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
    )

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test",
    "scala" % "virtualization-lms-core_2.10.0-virtualized-SNAPSHOT" % "0.1",
    "org.scala-lang" % "scala-compiler" % "2.10.0-virtualized-SNAPSHOT")
    
//--- End of Dependencies

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false
