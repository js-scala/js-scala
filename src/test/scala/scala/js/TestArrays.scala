package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait ArraysProg { this: LiftNumeric with NumericOps with Equal with Arrays =>
  def test1(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    a(1) = 4
    a.length + a(1) + x
  }

  def test2(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    val b = array(4, 5, 6)
    for (el <- a) {
      b(0) = el
    }
    b(0)
  }

  def test3(x: Rep[Int]): Rep[Array[Int]] = {
    val a = array(1, 2, 3)
    val b = for (el <- a) yield el+x
    b
  }

  def test4(x: Rep[Int]): Rep[Array[Int]] = {
    val a = array(1, 2, 3)
    val b = for (x1 <- a; x2 <- a) yield x1+x2
    b
  }

  def test5(x: Rep[Int]): Rep[Array[Int]] = {
    val a = array(1, 2, 3)
    val b = for (el <- a; if (el == x)) yield el
    b
  }

  def test6(n: Rep[Int]): Rep[Array[Int]] = {
    val a = array[Int]()
    for (i <- range(0, n)) {
      a(i) = i
    }
    a
  }

  def test7(n: Rep[Int]): Rep[Array[Int]] = {
    for (i <- range(0, n)) yield i
  }

  def test8(n: Rep[Int]): Rep[Array[Int]] = {
    for (i <- range(0, n); j <- range(0, n)) yield i*j
  }

  def test9(n: Rep[Int]): Rep[Array[Int]] = {
    for (i <- range(0, n); if (i == 1)) yield i*i
  }

  def test10(n: Rep[Int]): Rep[Array[Int]] = {
    val a = array(1, 2, 3)
    for (x1 <- a; i <- range(0, n); x2 <- a) yield (x1+x2)*i
  }
}

class TestArrays extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testArrays = {
    withOutFile(prefix+"arrays") {
    
      new ArraysProg with LiftNumeric with NumericOpsExpOpt with EqualExp with ArraysExp { self =>
        val codegen = new JSGenNumericOps with JSGenEqual with JSGenArrays { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new PrintWriter(System.out))
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3", new PrintWriter(System.out))
        codegen.emitSource(test4 _, "test4", new PrintWriter(System.out))
        codegen.emitSource(test5 _, "test5", new PrintWriter(System.out))
        codegen.emitSource(test6 _, "test6", new PrintWriter(System.out))
        codegen.emitSource(test7 _, "test7", new PrintWriter(System.out))
        codegen.emitSource(test8 _, "test8", new PrintWriter(System.out))
        codegen.emitSource(test9 _, "test9", new PrintWriter(System.out))
        codegen.emitSource(test10 _, "test10", new PrintWriter(System.out))
        val genLegacy = new JSGenNumericOps with JSGenEqual with JSGenArraysLegacy { val IR: self.type = self }
        genLegacy.emitSource(test2 _, "test2Legacy", new PrintWriter(System.out))
        genLegacy.emitSource(test3 _, "test3Legacy", new PrintWriter(System.out))
        genLegacy.emitSource(test5 _, "test5Legacy", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"arrays")
  }
}
