import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "EPFL",
    version := "0.4-SNAPSHOT",
    scalaVersion := "2.10.2-RC1",
    scalaOrganization := "org.scala-lang.virtualized",
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "root",
    file("core"),
    settings = buildSettings
  ) aggregate (macros, core)

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      scalaVersion := "2.10.2-SNAPSHOT",
      scalaOrganization := "org.scala-lang.macro-paradise",
      libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
        "EPFL" %% "lms" % "0.3-SNAPSHOT"
      )
    )
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-P:continuations:enable", "-Yvirtualize", "-language:dynamics", "-Ymacro-debug-lite"),
      name := "js-scala",
      //Our tests are not threadsafe so disabling parallel execution for now
      parallelExecution in Test := false,
      // disable publishing of main docs
      publishArtifact in (Compile, packageDoc) := false,
      // continuations
      autoCompilerPlugins := true,
    
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.0.M5b" % "test",
        "EPFL" %% "lms" % "0.3-SNAPSHOT"
      ),
      libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) => deps :+ compilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % ver)}
    )
  ) dependsOn (macros)
}


