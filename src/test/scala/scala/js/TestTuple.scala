package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait TwoArgsProg { this: NumericOps =>
  def test(x: Rep[Double], y: Rep[Double]): Rep[Double] = {
    x + y
  }
}

class TestTuple extends FileDiffSuite {

  val prefix = "test-out/"

  def testTwoArg = {
    withOutFile(prefix+"tuple") {
      new TwoArgsProg with TupleOpsExp with NumericOpsExpOpt { self =>
        val codegen = new JSTupledCodegen with JSGenNumericOps { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuple")
  }
}
