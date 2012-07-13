package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait LiteralProg { this: JSLiteral with LiftNumeric with NumericOps =>
  def test(x: Rep[Double]): Rep[Double] = {
    val o = new JSLiteral {
      val a = x
      val b = x + 2.0
      val c = 1.0
      val d = a + b
      val e = c + 2.0
    }
    o.a
  }
}

trait LiteralFunProg { this: JS =>
  def test(x: Rep[Int]): Rep[Int] = {
    val o = new JSLiteral {
      val a = x + 2
      val f = fun { (y : Rep[Int]) => y + a }
    }
    o.f(x)
  }
}

class TestLiteral extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testLiteral = {
    withOutFile(prefix+"literal") {
      new LiteralProg with JSLiteralExp with LiftNumeric with NumericOpsExpOpt { self =>
        val codegen = new JSNestedCodegen with JSGenLiteral with JSGenNumericOps { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"literal")
  }

  def testLiteralFun = {
    withOutFile(prefix+"literalfun") {
      new LiteralFunProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"literalfun")
  }

}
