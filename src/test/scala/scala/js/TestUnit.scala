package scala.js

import java.io.PrintWriter

import language.JS
import exp.JSExp
import gen.js.GenJS

trait UnitProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    // should return undefined
  }
}

class TestUnit extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testUnit = {
    withOutFile(prefix+"unit") {
      new UnitProg with JSExp { self =>
        val codegen = new GenJS { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"unit")
  }
}
