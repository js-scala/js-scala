# js.scala: JavaScript as an embedded DSL in Scala #

### Documentation

* ECOOP 2012 paper ([PDF](http://infoscience.epfl.ch/record/179888/files/js-scala-ecoop.pdf)) and slides ([PDF](http://pldi12.cs.purdue.edu/sites/default/files/slides_ecoop_gkossakowski.pdf))
* [Scala Days 2012 talk](http://skillsmatter.com/podcast/scala/javascript-embedded-dsl-scala)

### Setup

1. Setup [virtualization-lms-core](http://github.com/TiarkRompf/virtualization-lms-core):
  - `$ git clone git@github.com:TiarkRompf/virtualization-lms-core.git`
  - `$ cd virtualization-lms-core`
  - `$ sbt publish-local`
2. Clone the project and manage it using sbt:
  - `$ cd ..`
  - `$ git clone git@github.com:js-scala/js-scala.git`
  - `$ cd js-scala`
  - `$ sbt`
3. Run the tests:
  - `> test`
4. Publish it (if you want to use it in your project):
  - `> publish-local`
5. Run the examples:
  - `> project examples`
  - `> run`

### Use it in your project

1. Add a dependency on `js-scala-0.3-SNAPSHOT`
2. Set the Scala organization to `org.scala-lang.virtualized`
3. Set the `-Yvirtualize` compiler option

### Use it in Play! 2.1

Tweak your `project/Build.scala` file to look like the following:
```scala
object ApplicationBuild extends Build {

    val appName         = "foo"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "EPFL" %% "js-scala" % "0.3-SNAPSHOT"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      scalaOrganization := "org.scala-lang.virtualized",
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")
    )

}
```

### Use it in Play! 2.0.2

* [build-play20](http://github.com/js-scala/build-play20) builds Play 2! and all its transitive Scala dependencies with Scala-Virtualized, for compatibility with this DSL. Deployment on [dotcloud](http://github.com/js-scala/play2-on-dotcloud) is an option.

### Further projects

* [play-js-validation](http://github.com/js-scala/play-js-validation) uses this DSL to enable form validation code in Play 2.0 to be written once and checked on both client and server sides.

* [forest](http://github.com/js-scala/forest) uses this DSL to enable HTML templates to be written once and shared between client and server sides, both for initial rendering and automatic updating.
