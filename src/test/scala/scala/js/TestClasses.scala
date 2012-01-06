package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait ClassesProg { this: JS with JSClasses =>
  class Foo(v: Rep[Int]) {
    def f(): Rep[Int] = v
  }
  implicit def proxyRepFoo(x: Rep[Foo]) = repClassProxy[Foo](x, this)

  class Bar(w: Rep[Int]) extends Foo(w+1) {
    def g(): Rep[Int] = super.f() + w
  }
  implicit def proxyRepBar(x: Rep[Bar]) = repClassProxy[Bar](x, this)

  class Simple[A](private var value: Rep[A]) {
    def get() = value
    def set(value: Rep[A]) = (this.value = value)
  }
  implicit def proxyRepSimple[A:Manifest](x: Rep[Simple[A]]) = repClassProxy[Simple[A]](x, this)

  def testClassProxy(foo: Rep[Foo]): Rep[Int] = {
    foo.f()
  }

  def testReifiedClass(x: Rep[Int]): Rep[Int] = {
    val newFoo = register[Foo](this)
    val foo = newFoo(x)
    foo.f()
  }

  def testReifiedExtendedClass(x: Rep[Int]): Rep[Int] = {
    val newBar = register[Bar](this)
    val bar = newBar(x)
    bar.g() // 2x+1
  }

  def testGenericClassProxy(simple: Rep[Simple[Int]]): Rep[Int] = {
    simple.get()
  }

  def testGenericReifiedClass(x: Rep[Int]): Rep[Int] = {
    val newSimple = register[Simple[Int]](this)
    val simple = newSimple(0)
    simple.set(x)
    simple.get()
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

  def testReifiedClass = {
    withOutFile(prefix+"reified-class") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testReifiedClass _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"reified-class")
  }

  def testReifiedExtendedClass = {
    withOutFile(prefix+"reified-extended-class") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testReifiedExtendedClass _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"reified-extended-class")
  }

  def testGenericClassProxy = {
    withOutFile(prefix+"generic-class-proxy") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testGenericClassProxy _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"generic-class-proxy")
  }

  def testGenericReifiedClass = {
    withOutFile(prefix+"generic-reified-class") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testGenericReifiedClass _, "main", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix+"generic-reified-class")
  }
}
