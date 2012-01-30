name := "lms-sandbox"

organization := "EPFL"

version := "0.1"

//--- Local Scala

scalaHome <<= baseDirectory { f =>
  //val scalaVirtualizedHome = SettingKey[File]("scala-virtualized-home","Location of local Scala virtualized checkout")
  val props = new java.util.Properties()
  IO.load(props, f / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Did you forget to set scala.virtualized.home property in local.properties file?")
  else Some(file(x))
}

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalacOptions <+= scalaHome map (_.map(f => "-Xplugin:"+f+"/misc/scala-devel/plugins/continuations.jar").get)

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

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize")

//Our tests are not threadsafe so disabling parallel execution for now
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

