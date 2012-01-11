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

  trait Baz extends Bar {
    override def someMethod(): Rep[Int] = super.someMethod() + 1
  }
  implicit def proxyRepBaz(x: Rep[Baz]) = repProxy[Baz](x)

  def test(x: Rep[Int]): Rep[Int] = {
    val newFoo = registerTrait[Foo](this)
    val foo = newFoo()
    foo.someMethod() + x + foo.someVar + foo.someOtherMethod(x) // 2x + 3
  }

  def testExtends(x: Rep[Int]): Rep[Int] = {
    val newFoo = registerTrait[Foo](this)
    val newBar = registerTrait[Bar](this)
    val foo = newFoo()
    val bar = newBar()
    bar.someVar = 2
    bar.someNewMethod(x) + bar.someMethod() + foo.someVar // 2x + 4
  }

  def testDoubleExtends(x: Rep[Int]): Rep[Int] = {
    val newBaz = registerTrait[Baz](this)
    val baz = newBaz()
    baz.someMethod() // 3
  }
}

trait TraitsProgInScala extends TraitsProg with JSInScala with JSTraitsInScala { self =>
  override def createTrait[T<:AnyRef:Manifest](): T = {
    val m = implicitly[Manifest[T]]
    if      (m.equals(implicitly[Manifest[Foo]]))  (new Foo {}).asInstanceOf[T]
    else if (m.equals(implicitly[Manifest[Bar]]))  (new Bar {}).asInstanceOf[T]
    else if (m.equals(implicitly[Manifest[Baz]]))  (new Baz {}).asInstanceOf[T]
    else super.createTrait[T]()
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

  def testTraitsDoubleExtends = {
    withOutFile(prefix+"traits-double-extends") {
      new TraitsProg with JSExp with JSTraitsExp { self =>
        val codegen = new JSGen with JSGenTraits { val IR: self.type = self }
        codegen.emitSource(testDoubleExtends _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"traits-double-extends")
  }

  def testTraitsInScala = {
    new TraitsProgInScala { self =>
      expect(9){test(3)}
      expect(10){testExtends(3)}
      expect(3){testDoubleExtends(0)}
    }
  }
}
