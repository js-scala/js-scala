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
  implicit def proxyRepFoo(x: Rep[Foo]) = repProxy[Foo](x)

  trait Bar extends Foo {
    override def someMethod() : Rep[Int] = super.someMethod() + someVar
    def someNewMethod(i: Rep[Int]) : Rep[Int] = i*2
  }
  implicit def proxyRepBar(x: Rep[Bar]) = repProxy[Bar](x)

  def test(x: Rep[Int]): Rep[Int] = {
    val newFoo = register[Foo](this)
    val foo = newFoo()
    foo.someMethod() + x + foo.someVar + foo.someOtherMethod(x) // 2x + 3
  }

  def testExtends(x: Rep[Int]): Rep[Int] = {
    val newFoo = register[Foo](this)
    val newBar = register[Bar](this)
    val foo = newFoo()
    val bar = newBar()
    bar.someVar = 2
    bar.someNewMethod(x) + bar.someMethod() + foo.someVar // 2x + 4
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

  def testTraitsExtends = {
    withOutFile(prefix+"traits-extends") {
      new TraitsProg with JSExp with JSTraitsExp { self =>
        val codegen = new JSGen with JSGenTraits { val IR: self.type = self }
        codegen.emitSource(testExtends _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"traits-extends")
  }
}
