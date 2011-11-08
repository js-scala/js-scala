package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait TraitsProg { this: JS with JSTraits =>
  trait Foo {
    var someVar : Rep[Int] = 1
    def someMethod() : Rep[Int] = 1
    def someOtherMethod(i: Rep[Int]) : Rep[Int] = i+1
  }
  implicit def proxyRepFoo(x: Rep[Foo]) = proxyOps[Foo,Foo](x)

  def test(x: Rep[Int]): Rep[Int] = {
    val newFoo = register[Foo](this)
    val foo = newFoo()
    foo.someMethod() + x + foo.someVar + foo.someOtherMethod(x)
  }
}

class TestTraits extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testTraits = {
    withOutFile(prefix+"traits") {
      new TraitsProg with JSExp with JSTraitsExp { self =>
        val codegen = new JSGen with JSGenTraits { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"traits")
  }
}
