package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait TwoArgsProg { this: NumericOps =>
  def test(x: Rep[Double], y: Rep[Double]): Rep[Double] = {
    x + y
  }
}

trait TwoArgsFunProg { this: JSFunctions with NumericOps =>
  def test(x: Rep[Int]): Rep[Int] = {
    val f = fun { (a : Rep[Int], b : Rep[Int]) => a + b }
    f(x, x)
  }
}

trait NoBadUnboxingProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    def f = fun { (a: Rep[Any]) => a }
    f(make_tuple2[Int,Int](1, 2))
  }
}

class TestTuple extends FileDiffSuite {

  val prefix = "test-out/"

  def testTwoArgs = {
    withOutFile(prefix+"tuple") {
      new TwoArgsProg with TupleOpsExp with NumericOpsExpOpt { self =>
        val codegen = new JSTupledCodegen with JSGenNumericOps { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuple")
  }

  def testTwoArgsFun = {
    withOutFile(prefix+"tuplefun") {
      new TwoArgsFunProg with JSFunctionsExp with NumericOpsExpOpt { self =>
        val codegen = new JSGenFunctions with JSGenNumericOps with JSGenTupleOps with GenericGenUnboxedTupleAccess { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuplefun")
  }

  def testNoBadUnboxing = {
    withOutFile(prefix+"nobadunboxing") {
      new NoBadUnboxingProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"nobadunboxing")
  }
}
