package scala.js

import scala.virtualization.lms.common._
import scala.util.continuations._

import java.io.PrintWriter
import java.io.FileOutputStream

trait CPSProg { this: LiftNumeric with Arrays with JSDebug with LiftString with CPS =>

  def sleep(delay: Int) = shift { retrn: (Rep[Unit]=>Rep[Unit]) =>
    alert("slept for a while...")
    retrn()
  }

  def test1(x: Rep[Int]): Rep[Unit] = reset {
    val xs = array(1, 2, 3)
      for (x <- richArray(xs).suspendable) {
        sleep(0)
        alert(String.valueOf(x))
      }
    alert("done")
  }
  
  def test2(x: Rep[Int]): Rep[Unit] = reset {
   val xs = array(4, 5, 6)
   val ys = for (x <- richArray(xs).suspendable) yield {
     sleep(0)
     x+1
    }
    alert(String.valueOf(ys))
  }

}

class TestCPS extends FileDiffSuite {

  val prefix = "test-out/"

  def testArrays = {
    withOutFile(prefix+"cps") {

      new CPSProg with LiftNumeric with ArraysExp with JSDebugExp with LiftString with CPSExp with JSExp { self =>
        val codegen = new JSGenArrays with JSGenDebug with JSGenVariables with JSGen { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new PrintWriter(System.out))
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"cps")
  }
}
