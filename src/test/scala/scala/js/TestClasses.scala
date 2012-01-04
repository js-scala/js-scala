package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait ClassesProg { this: JS with JSClasses =>
  class Foo(v: Rep[Int]) {
    def f(): Rep[Int] = v
  }
  implicit def proxyRepFoo(x: Rep[Foo]) = repClassProxy[Foo](x, this)

  def testClassProxy(foo: Rep[Foo]): Rep[Int] = {
    foo.f()
  }
}

class TestClasses extends FileDiffSuite {
  val prefix = "test-out/"
  def testClassProxy = {
    withOutFile(prefix+"class-proxy") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testClassProxy _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"class-proxy")
  }
}
