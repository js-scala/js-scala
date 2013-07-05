package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.js.gen.js.GenVariables

trait VariablesProg { this: LiftVariables with Variables with LiftNumeric  =>
  def test(x: Rep[Int]): Rep[Int] = {
    var v = 0
    v = x
    v += 1
    v
  }
}

class TestVariables extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testVariables = {
    withOutFile(prefix+"variables") {
      new VariablesProg with LiftVariables with VariablesExp with LiftNumeric { self =>
        val codegen = new GenVariables { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"variables")
  }
}
