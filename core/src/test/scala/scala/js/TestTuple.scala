package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.js.gen.js._
import exp.JSExp
import language.JS

trait TwoArgsProg { this: NumericOps =>
  def test(x: Rep[Double], y: Rep[Double]): Rep[Double] = {
    x + y
  }
}

trait TwoArgsFunProg { this: TupledFunctions with NumericOps =>
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

trait UnboxedQuoteProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    def f = fun { (a: Rep[(Int, Int)]) => a }
    f(make_tuple2[Int,Int](1, 2))
  }
}

trait SimpleTupleProg { this: JS =>
  def test(x: Rep[Any]): (Rep[Int], Rep[String], Rep[Boolean]) = {
    (1, "2", true)
  }
}

class TestTuple extends FileDiffSuite {

  val prefix = "test-out/"

  def testTwoArgs = {
    withOutFile(prefix+"tuple") {
      new TwoArgsProg with TupleOpsExp with NumericOpsExpOpt with PrimitiveOpsExp { self =>
        val codegen = new NestedCodegen with GenTupleOps with GenNumericOps { val IR: self.type = self }
        codegen.emitSource2(test, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuple")
  }

  def testTwoArgsFun = {
    withOutFile(prefix+"tuplefun") {
      new TwoArgsFunProg with TupledFunctionsRecursiveExp with NumericOpsExpOpt with PrimitiveOpsExp { self =>
        val codegen = new GenFunctions with GenNumericOps with GenTupleOps with GenericGenUnboxedTupleAccess { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuplefun")
  }

  def testNoBadUnboxing = {
    withOutFile(prefix+"nobadunboxing") {
      new NoBadUnboxingProg with JSExp { self =>
        val codegen = new GenJS { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"nobadunboxing")
  }

  def testUnboxedQuote = {
    withOutFile(prefix+"unboxedquote") {
      new UnboxedQuoteProg with JSExp { self =>
        val codegen = new GenJS { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"unboxedquote")
  }

  def testObjectTranslation = {
    withOutFile(prefix+"tuple-as-object") {
      new SimpleTupleProg with JSExp { self =>
        val codegen = new GenJS { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"tuple-as-object")
  }

//  def testArrayTranslation = {
//    withOutFile(prefix+"tuple-as-array") {
//      new SimpleTupleProg with JSExp { self =>
//        val codegen = new GenJS {
//          val IR: self.type = self
//          override def tupleAsArrays = true
//        }
//        codegen.emitSource(test _, "main", new PrintWriter(System.out))
//      }
//    }
//    assertFileEqualsCheck(prefix+"tuple-as-array")
//  }

}
