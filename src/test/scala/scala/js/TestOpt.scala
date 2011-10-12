package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait OptProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    val x_ = dynamic(x)
    x_.foo()
    x_.bar.baz()
    if (x_.test().asInstanceOf[Rep[Boolean]]) x_.ok()
    else x_.boo()
    x_.ret
  }
}

class TestOpt extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testOpt = {
    withOutFile(prefix+"opt") {
      new OptProg with JSExp { self =>
        val codegen = new JSGenOpt { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"opt")
  }
}
