name := "lms-sandbox"

organization := "EPFL"

version := "0.1"

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

//--- Paths

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

resourceDirectory in Compile <<= baseDirectory(_ / "resources")

scalaSource in Test <<= baseDirectory(_ / "test-src")

resourceDirectory in Test <<= baseDirectory(_ / "test-resources")

//--- End of Paths

//--- Dependencies

resolvers ++= Seq(
    ScalaToolsSnapshots, 
    //needed for custom build of scala test
    "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
    )

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test",
    "scala" % "virtualization-lms-core_2.10.0-virtualized-SNAPSHOT" % "0.1")
    
//--- End of Dependencies

scalacOptions ++= Seq("-deprecation", "-Xexperimental")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false
