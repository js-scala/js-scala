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

`greet` is a JavaScript program generator producing a program that prints a message in the console:

```javascript
function greet(x0) {
  var x1 = "Hello, "+x0;
  var x2 = x1+"!";
  var x3 = console.log(x2);
}
```

Note: some language units also support server-side code generation (see the [Quick start](#quick-start) section below), allowing code sharing between client and server sides.

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
5. Run the examples:
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

### Write a program generator

Write your program generator in a trait extending the language units you want to use:

```scala
import scala.js.language.JS // JavaScript-like language unit
import scala.js.language.dom.Dom // DOM API

trait Program extends JS with Dom {

  def printClicks() = {
    for (target <- document.find("#target")) {
      target.on(Click) { click =>
        println("click at (" + click.offsetX + ", " + click.offsetY + ")")
      }
    }
  }

}
```

The `Program` trait contains a method `printClicks` that describes a JavaScript program searching for an element with id “target”, attaching it a “click” event listener and printing the mouse coordinates on each click.

### Generate JavaScript code

To generate a JavaScript program from the above program description, write a Scala application instantiating your program generator and creating a code generator with the corresponding language unit implementations:

```scala
import scala.js.exp.JSExp // JS language unit implementation
import scala.js.exp.dom.DomExp // Dom language unit implementation
import scala.js.gen.js.GenJS // JS JavaScript code generator
import scala.js.gen.js.dom.GenDom // Dom JavaScript code generator

object Generator extends App {
  // instantiate the program generator with language unit implementations
  val program = new Program with JSExp with DomExp
  // create a code generator for these language units and pass it the program as a parameter
  val gen = new GenJS with GenDom { val IR: program.type = program }

  // emit the code of the program described by the printClicks method
  gen.emitExecution(program.printClicks(), new java.io.PrintWriter("printclicks.js"))
}
```

The above code generator will print the following JavaScript program in file `printclicks.js`:

```javascript
(function () {
  var x0 = document.querySelector("#target");
  if (x0 !== null) {
    var x1 = x0;
    x1.addEventListener('click', function (x2) {
      var x3 = x2.offsetX;
      var x4 = "click at ("+x3;
      var x5 = x4+", ";
      var x6 = x2.offsetY;
      var x7 = x5+x6;
      var x8 = x7+")";
      var x9 = console.log(x8);
    }, false);
  }
  var x13 = undefined;
}
```

### Go a bit further

Some language unit implementations also have a trait defining optimizations. When such a trait exists it is named like the language unit implementation trait with the additional suffix `Opt`. For instance, there is a `DomExpOpt` trait for the `Dom` language unit. If you use it in the above example instead of `DomExp`, it will generate `document.getElementById("target")` instead of `document.querySelector("#target")`.

Some language units also have Scala code generators, allowing code using them to be shared by the client-side and the server-side (see next section for more details on this).

The browser API is not exactly the same as the native API. It tries to be as most type safe as possible and sometimes uses shorter names (e.g. `on` instead of `addEventListener`).

### js-scala modules layout

The modules defined by js-scala generally stick to the following conventions:

- *language units* are defined under the `scala.js.language` package (though some language units are already provided by LMS under the `scala.virtualization.lms.common` package) ;
- *implementations* are defined under the `scala.js.exp` package. A language unit `Foo` is implemented by a module named `FooExp` (another module defining optimizations can also be defined under the name `FooExpOpt`) ;
- *code generators* are defined under the `scala.js.gen.js` and `scala.js.gen.scala` packages, for JavaScript and Scala, respectively. A code generator for a language unit `Foo` has name `GenFoo`.

A language unit is a trait usually named according to a concept (e.g. `Arrays`, `Adts`, etc.) or to a type suffixed by “Ops” (e.g. `OptionOps`, `ListOps`, etc.). Mix such traits in your code to import their *vocabulary*. This vocabulary can consist of top-level functions (e.g. `fun`, provided by the `Functions` language unit, or `array`, provided by `Arrays`), or methods implicitly added to `Rep[X]` values where “X” is (generally) the name of the language unit without the trailing `Ops` (e.g. the `ElementOps` trait implicitly adds methods on `Rep[Element]` values, the `BooleanOps` trait implicitly adds methods on `Rep[Boolean]` values).

The `JsScala` language unit defines a core language supporting the following concepts or types:

- `if`, `while`, `==`, `var`, tuples (using Scala syntax) ;
- “primitive types” (`Int`, `Long`, `Double`, `Float`, `Boolean`), `String`, `List` ;
- functions (using the term `fun` instead of Scala’s `def`) ;
- `println`.

This trait has code generators for both Scala and JavaScript, meaning that you can generate Scala and JavaScript programs from the same code.

The `JS` trait extends `JsScala` with JavaScript specific language units: arrays, dynamic typing, regexps.

The `Dom` trait provides Web browsers APIs (e.g. the `window` and `document` objects, a selector API, etc.).