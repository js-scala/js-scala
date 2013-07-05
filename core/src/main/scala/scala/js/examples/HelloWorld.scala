package scala.js
package examples

import scala.js.language.JsScala
import scala.js.exp.JsScalaExp
import scala.js.gen.js.{GenJsScala => JSGenJsScala}
import scala.js.gen.scala.{GenJsScala => ScalaGenJsScala}

object HelloWorld {

  trait Prog extends JsScala {
    def main(name: Rep[String]): Rep[Unit] = {
      println("Hello, " + name + "!")
    }
  }

  val prog = new Prog with JsScalaExp

  def printJs() {
    val jsGen = new JSGenJsScala { val IR: prog.type = prog }
    jsGen.emitSource(prog.main, "main", new java.io.PrintWriter(System.out))
  }

  def printScala() {
    val scalaGen = new ScalaGenJsScala { val IR: prog.type = prog }
    scalaGen.emitSource(prog.main, "main", new java.io.PrintWriter(System.out))
  }
}
