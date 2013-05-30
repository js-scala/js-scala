package scala.js

import scala.virtualization.lms.common._
import scala.util.continuations._
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.js.language.JSLib
import scala.js.language.JS
import scala.js.language.CPS
import scala.js.language.Casts
import scala.js.language.Ajax
import scala.js.language.Debug
import scala.js.exp.AjaxExp
import scala.js.exp.CPSExp
import scala.js.exp.DebugExp
import scala.js.exp.JSLibExp
import scala.js.exp.JSExp
import scala.js.gen.js.GenJSLib
import scala.js.gen.js.GenAjax
import scala.js.gen.js.GenCPS
import scala.js.gen.js.GenJS
import scala.js.gen.js.GenDebug

trait CPSProg { this: JS with Debug with JSLib with CPS with Ajax with Casts =>

  def sleep(delay: Rep[Int]) = shift { retrn: (Rep[Unit]=>Rep[Unit]) =>
    log("sleeping for " + delay)
    window.setTimeout(retrn, delay)
    unit(())
  }

  def test1(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(1, 2, 3)
      for (x <- xs.suspendable) {
        sleep(x * 1000)
        log(String.valueOf(x))
      }
    log("done")
  }
  
  def test2(x: Rep[Int]): Rep[Unit] = reset {
   val xs = array(4, 5, 6)
   val ys = for (x <- xs.suspendable) yield {
     sleep(x * 1000)
     x+1
    }
    log(String.valueOf(ys))
  }
  
  def test3(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(1, 2, 3)
      for (x <- xs.parSuspendable) {
        sleep(x * 1000)
        log(String.valueOf(x))
      }
    log("done")
  }

  def test3b(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(3, 2, 1)
    val ys = for (x <- xs.parSuspendable) yield {
        sleep(x * 1000)
        log(String.valueOf(x))
        x+1
      }
    log(String.valueOf(ys))
  }

  def test3c(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(3, 2, 1)
      for (x <- xs.parSuspendable) {
        sleep(x * 1000)
        log(String.valueOf(x))
      }
    log("done")
  }
  
  def test4(x: Rep[Int]): Rep[Unit] = reset {
    for (user <- array("gkossakowski", "odersky", "adriaanm").parSuspendable) {
      log("fetching " + user)
      val data = ajax.get {
        new Record {
          val url = "http://api.twitter.com/1/statuses/user_timeline.json"
          val `type` = "GET"
          val dataType = "jsonp"
          val data = new Record {
            val screen_name = user
            val include_rts = true
            val count = 5
            val include_entities = true
          }
        }
      }
      val tweets = data.as[Array[Record {val text: String}]]
      for (t <- tweets)
        log("fetched " + t.text)
    }
  }

}

class TestCPS extends FileDiffSuite {

  val prefix = "test-out/"

  def testArrays = {
    withOutFile(prefix+"cps") {

      new CPSProg with JSExp with DebugExp with JSLibExp with CPSExp with AjaxExp with Casts { self =>
        val codegen = new GenJS with GenDebug with GenJSLib with GenCPS with GenAjax { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new PrintWriter(System.out))
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3", new PrintWriter(System.out))
        codegen.emitSource(test3b _, "test3b", new PrintWriter(System.out))
        codegen.emitSource(test3c _, "test3c", new PrintWriter(System.out))
        codegen.emitSource(test4 _, "test4", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"cps")
  }
}
