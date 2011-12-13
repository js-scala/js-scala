package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait RefinementCastProg { this: Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[{val foo: String; val bar: Int}]
  }
}

trait JSLiteralCastProg { this: JS with Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[JSLiteral {val foo: String; val bar: Int}]
  }
}

trait NestedJSLiteralCastProg { this: JS with Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[JSLiteral {val foo: String; val lit: JSLiteral { val a: Int }}]
  }
}

class TestCasts extends FileDiffSuite {

  val prefix = "test-out/"

  def testRefinement = {
    withOutFile(prefix+"refinement_cast") {
      new RefinementCastProg with CastsCheckedExp { self =>
        val codegen = new GenCastChecked { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"refinement_cast")
  }

  def testJSLiteral = {
    withOutFile(prefix+"jsliteral_cast") {
      new JSLiteralCastProg with JSExp with CastsCheckedExp { self =>
        val codegen = new GenCastChecked { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"jsliteral_cast")
  }
  
  def testNestedJSLiteral = {
    withOutFile(prefix+"jsliteral_nested_cast") {
      new NestedJSLiteralCastProg with JSExp with CastsCheckedExp { self =>
        val codegen = new GenCastChecked { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"jsliteral_nested_cast")
  }

}
