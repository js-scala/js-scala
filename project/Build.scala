import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "EPFL",
    version := "0.4-SNAPSHOT",
    scalaVersion := "2.10.2-RC1",
    scalaOrganization := "org.scala-lang.virtualized",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scala-lang.virtualized.plugins" % "macro-paradise_2.10.2-RC1" % "2.0.0-SNAPSHOT")
  )
}

object JsScalaBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      publishLocal := ()
    )
  ) aggregate (core, examples)

  lazy val core = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics"/*, "-Ymacro-debug-lite"*/),

      name := "js-scala",

      //Our tests are not threadsafe so disabling parallel execution for now
      parallelExecution in Test := false,
      // disable publishing of main docs
      publishArtifact in (Compile, packageDoc) := false,
      // continuations
      autoCompilerPlugins := true,
    
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
        "EPFL" %% "lms" % "0.3-SNAPSHOT",
        "org.scala-lang.virtualized" % "scala-reflect" % scalaVersion.value,
        compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % scalaVersion.value)
      )
    )
  )

  lazy val examples = Project(
    "examples",
    file("examples"),
    settings = buildSettings ++ Seq(
      publishLocal := (),
      autoCompilerPlugins := true,
      libraryDependencies += compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % scalaVersion.value),
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics")
    )
  ) dependsOn core

}


