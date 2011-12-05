package scala.js

import scala.virtualization.lms.common._
import scala.util.continuations._

import java.io.PrintWriter
import java.io.FileOutputStream

trait CPSProg { this: JS with JSDebug with JSLib with CPS =>

  def sleep(delay: Rep[Int]) = shift { retrn: (Rep[Unit]=>Rep[Unit]) =>
    log("sleeping for " + delay)
    window.setTimeout(retrn, delay)
    unit(())
  }

  def test1(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(1, 2, 3)
      for (x <- richArray(xs).suspendable) {
        sleep(x * 1000)
        log(String.valueOf(x))
      }
    log("done")
  }
  
  def test2(x: Rep[Int]): Rep[Unit] = reset {
   val xs = array(4, 5, 6)
   val ys = for (x <- richArray(xs).suspendable) yield {
     sleep(x * 1000)
     x+1
    }
    log(String.valueOf(ys))
  }
  
  def test3(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(1, 2, 3)
      for (x <- richArray(xs).parSuspendable) {
        sleep(x * 1000)
        log(String.valueOf(x))
      }
    log("done")
  }

}

class TestCPS extends FileDiffSuite {

  val prefix = "test-out/"

  def testArrays = {
    withOutFile(prefix+"cps") {

      new CPSProg with JSExp with JSDebugExp with JSLibExp with CPSExp { self =>
        val codegen = new JSGen with JSGenDebug with JSGenLib with GenCPS { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new PrintWriter(System.out))
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"cps")
  }
}
