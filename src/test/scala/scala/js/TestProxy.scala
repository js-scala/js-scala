package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait TestProxyDummy extends JSProxyBase {
  trait Dummy {
    var someVar: Rep[Int]
    def someMethod(n: Rep[Int]): Rep[Int]
    def someGetter(): Rep[Int]
  }
  implicit def repToDummy(x: Rep[Dummy]): Dummy = repProxy[Dummy](x)
}

trait TestProxyDummyExp extends TestProxyDummy with JSProxyExp

trait ProxyProg { this: JS with TestProxyDummy =>
  def test(x: Rep[Dummy]): Rep[Int] = {
    x.someVar = x.someMethod(1) + x.someVar + x.someGetter()
    x.someVar
  }
}

class TestProxy extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testProxy = {
    withOutFile(prefix+"proxy") {
      new ProxyProg with JSExp with TestProxyDummyExp { self =>
        val codegen = new JSGen with JSGenProxy { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"proxy")
  }
}
