name := "js-scala-benchmarks"

organization := "julienrf"

scalaVersion := "2.10.1"

scalaOrganization := "org.scala-lang.virtualized"

scalacOptions ++= Seq("-Xlint", "-Yvirtualize", "-deprecation", "-feature")

libraryDependencies += "EPFL" %% "js-scala" % "0.4-SNAPSHOT"
