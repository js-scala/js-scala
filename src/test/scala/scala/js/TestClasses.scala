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

  class Simple[A](var value: Rep[A]) {
    def get() = value
    def set(value: Rep[A]) = (this.value = value)
  }
  implicit def proxyRepSimple[A:Manifest](x: Rep[Simple[A]]) = repClassProxy[Simple[A]](x, this)

  class Counter(private var v: Rep[Int]) {
    def get() = v
    private def set(v: Rep[Int]) = (this.v = v)
    def inc() = { set(get()+1); get() }
  }
  implicit def proxyRepCounter(x: Rep[Counter]) = repClassProxy[Counter](x, this)

  class Bloat(v: Rep[Int]) {
    def get() = ({ () => v })()
  }
  implicit def proxyRepBloat(x: Rep[Bloat]) = repClassProxy[Bloat](x, this)

  class FancyPair[A:Manifest,B:Manifest](a: Rep[A], b: Rep[B]) {
    private var t: Rep[(A,B)] = make_tuple2(a,b)
    def fst() = tuple2_get1(t)
    def snd() = tuple2_get2(t)
  }
  implicit def proxyRepFancyPair[A:Manifest,B:Manifest](x: Rep[FancyPair[A,B]]) = repClassProxy[FancyPair[A,B]](x, this)

  class Queue[A:Manifest] {
    private var a = array[A]()
    private var s = unit(0)
    private var e = unit(0)
    def put(x: Rep[A]) = { a(e) = x; e += 1 }
    def get() = { s += 1; a(s-1) }
  }
  implicit def proxyRepQueue[A:Manifest](x: Rep[Queue[A]]) = repClassProxy[Queue[A]](x, this)

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

  def testPrivateReifiedClass(x: Rep[Int]): Rep[Int] = {
    val newCounter = register[Counter](this)
    val counter = newCounter(x)
    counter.inc()
    counter.inc() // x+2
  }

  def testBloat(x: Rep[Int]): Rep[Int] = {
    val newBloat = register[Bloat](this)
    val bloat = newBloat(x)
    bloat.get()
  }

  def testManifestClassProxy(fp: Rep[FancyPair[Int,Int]]): Rep[Int] = {
    fp.fst() + fp.snd()
  }

  def testManifestReifiedClass(x: Rep[Int]): Rep[Int] = {
    val newFancyPair = register[FancyPair[Int,Int]](this)
    val fp = newFancyPair(x,x+1)
    fp.fst() + fp.snd() // 2x+1
  }

  def testQueueProxy(queue: Rep[Queue[Int]]): Rep[Int] = {
    val x = 0
    queue.put(x)
    queue.put(x+1)
    queue.put(x+2)
    queue.get()
    queue.get()
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
    assertFileEqualsCheck(prefix+"generic-reified-class")
  }

  def testPrivateReifiedClass = {
    withOutFile(prefix+"private-reified-class") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testPrivateReifiedClass _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"private-reified-class")
  }

  def testReifiedClassBloat = {
    withOutFile(prefix+"reified-class-bloat") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testBloat _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"reified-class-bloat")
  }

  def testManifestClassProxy = {
    withOutFile(prefix+"manifest-class-proxy") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testManifestClassProxy _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"manifest-class-proxy")
  }

  def testManifestReifiedClass = {
    withOutFile(prefix+"manifest-reified-class") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testManifestReifiedClass _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"manifest-reified-class")
  }

  def testClassProxyQueue = {
    withOutFile(prefix+"class-proxy-queue") {
      new ClassesProg with JSExp with JSClassesExp { self =>
        val codegen = new JSGen with JSGenClasses { val IR: self.type = self }
        codegen.emitSource(testQueueProxy _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"class-proxy-queue")
  }
}
