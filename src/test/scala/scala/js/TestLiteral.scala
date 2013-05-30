package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import java.io.FileOutputStream
import org.scalatest.Ignore
import language.JS
import exp.JSExp
import gen.js.{NestedCodegen, GenStruct, GenNumericOps, GenJS}

trait LiteralProg { this: Structs with LiftNumeric with NumericOps =>
  def test(x: Rep[Double]): Rep[Double] = {
    val o = new Record {
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
    val o = new Record {
      val a = x + 2
      val f = fun { (y : Rep[Int]) => y + a }
    }
    val f = o.f
    f(x)
  }
}

class TestLiteral extends FileDiffSuite {
  val prefix = "test-out/"

  // Ignore these tests until self references are handled by structs
  @Ignore def testLiteral = {
    withOutFile(prefix+"literal") {
      new LiteralProg with StructExp with LiftNumeric with NumericOpsExpOpt { self =>
        val codegen = new NestedCodegen with GenStruct with GenNumericOps { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"literal")
  }

  @Ignore def testLiteralFun = {
    withOutFile(prefix+"literalfun") {
      new LiteralFunProg with JSExp { self =>
        val codegen = new GenJS { val IR: self.type = self }
        codegen.emitSource(test _, "test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"literalfun")
  }

}
