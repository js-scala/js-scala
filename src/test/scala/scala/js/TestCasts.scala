package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.js.language.Casts
import scala.js.language.JS
import scala.js.exp.CastsCheckedExp
import scala.js.gen.js.GenCastChecked
import scala.js.exp.JSExp

trait RefinementCastProg { this: Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[{val foo: String; val bar: Int}]
  }
}

trait JSLiteralCastProg { this: JS with Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[Record {val foo: String; val bar: Int}]
  }
}

trait NestedJSLiteralCastProg { this: JS with Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[Record {val foo: String; val lit: Record { val a: Int }}]
  }
}

trait ArrayOfJSLiteralCastProg { this: JS with Casts =>
  def test(x: Rep[Any]): Rep[Any] = {
    x.as[Array[Record {val data: String}]]
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
  
  def testArrayJSLiteral = {
    withOutFile(prefix+"jsliteral_array_cast") {
      new ArrayOfJSLiteralCastProg with JSExp with CastsCheckedExp { self =>
        val codegen = new GenCastChecked { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"jsliteral_array_cast")
  }

}
