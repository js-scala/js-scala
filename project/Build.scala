import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
	version := "1.0.0",
    scalacOptions ++= Seq(),
    scalaVersion := "2.10.1",
    scalaOrganization := "org.scala-lang",
    resolvers += Resolver.sonatypeRepo("snapshots"),
	//mainClass := Some("prog.Main"),
	//Our tests are not threadsafe so disabling parallel execution for now
    parallelExecution in Test := false,
    // disable publishing of main docs
    publishArtifact in (Compile, packageDoc) := false,
    // continuations
    autoCompilerPlugins := true
	
   )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
	  scalaVersion := "2.10.2-SNAPSHOT",
      scalaOrganization := "org.scala-lang.macro-paradise",
      libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _),
	  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.0.M5b" % "test", "EPFL" %% "lms" % "0.3-SNAPSHOT")
	  )
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      version := "0.4-SNAPSHOT",
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics", "-Ymacro-debug-lite"),
	  scalaOrganization := "org.scala-lang.virtualized",
	  
	libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.0.M5b" % "test", "EPFL" %% "lms" % "0.3-SNAPSHOT"),
	libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) => deps :+ compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % ver)}
 )
  ) dependsOn(macros)
}


