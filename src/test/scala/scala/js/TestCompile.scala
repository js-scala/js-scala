package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait WhileProg { this: JS =>
  def test(n: Rep[Int]): Rep[Int] = {
    var x = 0
    while(x < n) { x += 1 }
    x
  }
}

class TestCompile extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testWhile = {
    withOutFile(prefix+"while") {
      new WhileProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"while")
  }
}
