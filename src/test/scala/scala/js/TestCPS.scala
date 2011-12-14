package scala.js

import scala.virtualization.lms.common._
import scala.util.continuations._

import java.io.PrintWriter
import java.io.FileOutputStream

trait CPSProg { this: JS with JSDebug with JSLib with CPS with Ajax with Casts =>

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
        new JSLiteral {
          val url = "http://api.twitter.com/1/statuses/user_timeline.json"
          val `type` = "GET"
          val dataType = "jsonp"
          val data = new JSLiteral {
            val screen_name = user
            val include_rts = true
            val count = 5
            val include_entities = true
          }
        }
      }
      val tweets = data.as[Array[JSLiteral {val text: String}]]
      for (t <- tweets)
        log("fetched " + t.text)
    }
  }

}

class TestCPS extends FileDiffSuite {

  val prefix = "test-out/"

  def testArrays = {
    withOutFile(prefix+"cps") {

      new CPSProg with JSExp with JSDebugExp with JSLibExp with CPSExp with AjaxExp with Casts { self =>
        val codegen = new JSGen with JSGenDebug with JSGenLib with GenCPS with GenAjax { val IR: self.type = self }
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
