package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait LiteralProg { this: JSLiteral with LiftNumeric with NumericOps =>
  def test(x: Rep[Double]): Rep[Double] = {
    val o = new JSLiteral {
      val a = x
      val b = x + 2.0
      // TODO: A bug in reified new logic. We need explicit an type
      //       annotation for now.
      val c: Double = 1.0
      val d = a + b
      val e = c + 2.0
    }
    o.a
  }
}


class TestLiteral extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testLiteral = {
    withOutFile(prefix+"literal") {
      new LiteralProg with JSLiteralExp with LiftNumeric with NumericOpsExpOpt { self =>
        val codegen = new JSGenLiteral with JSGenNumericOps { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"literal")
  }
}
