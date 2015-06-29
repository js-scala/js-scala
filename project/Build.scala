import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "EPFL",
    version := "0.4-SNAPSHOT",
    scalaVersion := "2.11.2",
    scalaOrganization := "org.scala-lang.virtualized",
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
}

object JsScalaBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "js-scala",
    file("."),
    settings = buildSettings ++ Seq(
      publishLocal := ()
    )
  ) aggregate (core, examples)

  lazy val core = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics"/*, "-Ymacro-debug-lite"*/),

      name := "js-scala",

      //Our tests are not threadsafe so disabling parallel execution for now
      parallelExecution in Test := false,
      // disable publishing of main docs
      publishArtifact in (Compile, packageDoc) := false,
      // continuations
      autoCompilerPlugins := true,
    
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.2" % "test",
        "EPFL" %% "lms" % "0.3-SNAPSHOT",
        "org.scala-lang.virtualized" % "scala-reflect" % scalaVersion.value,
        compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

      )
    )
  )

  lazy val examples = Project(
    "examples",
    file("examples"),
    settings = buildSettings ++ Seq(
      publishLocal := (),
      autoCompilerPlugins := true,
      libraryDependencies += compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2"),

      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics")
    )
  ) dependsOn core

}


