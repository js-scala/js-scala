# js-scala: JavaScript as an embedded DSL in Scala #

js-scala is a Scala library providing composable JavaScript code generators as embedded DSLs. Generate (optimized) JavaScript code from Scala-like code:

```scala
import scala.js.language.JS
trait JavaScriptGreet extends JS {

  def greet(name: Rep[String]): Rep[Unit] = {
    println("Hello, " + name + "!")
  }

}
```

`greet` is a JavaScript program generator that produces a program that prints a message in the console. The JavaScript code can be produced as follows:

```scala
import scala.js.exp.JSExp
import scala.js.gen.js.GenJS
object Generator extends App {
  val javaScriptGreet = new JavaScriptGreet with JSExp
  val codeGen = new GenJS { val IR: javaScriptGreet.type = javaScriptGreet }
  codeGen.emitSource(javaScriptGreet.greet, "greet", new java.io.PrintWriter(System.out))
}
```

Running the above Scala program will print the following on the standard output:

```javascript
function greet(x0) {
  var x1 = "Hello, "+x0;
  var x2 = x1+"!";
  var x3 = console.log(x2);
}
```

## Publications and talks

* ECOOP 2012 paper ([PDF](http://infoscience.epfl.ch/record/179888/files/js-scala-ecoop.pdf)) and slides ([PDF](http://pldi12.cs.purdue.edu/sites/default/files/slides_ecoop_gkossakowski.pdf))
* [Scala Days 2012 talk](http://skillsmatter.com/podcast/scala/javascript-embedded-dsl-scala)
* mloc-js'13 talk ([slides](http://prezi.com/l23gghh7c27t/?utm_campaign=share&utm_medium=copy&rc=ex0share))
* GPCE'13 paper ([PDF](https://github.com/js-scala/js-scala/raw/master/papers/gpce2013/gpce19c-foy.pdf)) and [slides](https://docs.google.com/presentation/d/1ErPjZMTheuKwp428QpxWibZlyjK3PKijwKtYEaXWoxQ/pub?start=false&loop=false&delayms=3000)

## Setup

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
5. Generate the API documentation:
  - `> doc`
  - The documentation is generated in the `core/target/scala-2.10/api/` directory.
6. Run the examples:
  - `> project examples`
  - `> run`

## Use it in your project

1. Add a dependency on js-scala 0.4-SNAPSHOT
  - `libraryDependencies += "EPFL" %% "js-scala" % "0.4-SNAPSHOT"`
2. Use Scala 2.10.2-RC1 and set the Scala organization to `org.scala-lang.virtualized`
  - `scalaVersion := "2.10.2-RC1"`
  - `scalaOrganization := "org.scala-lang.virtualized"`
3. Set the `-Yvirtualize` compiler option
  - `scalacOptions += "-Yvirtualize"`

## Further projects

* [play-js-validation](http://github.com/js-scala/play-js-validation) uses this DSL to enable form validation code in Play 2.0 to be written once and checked on both client and server sides.

* [forest](http://github.com/js-scala/forest) uses this DSL to enable HTML templates to be written once and shared between client and server sides, both for initial rendering and automatic updating.

## Quick start

First, be sure to be familiar with [LMS tutorials](http://scala-lms.github.io/tutorials).

(More to come!)