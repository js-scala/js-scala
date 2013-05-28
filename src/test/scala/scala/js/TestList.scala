package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.js.gen.js.GenNumericOps
import scala.js.gen.js.GenListOps
import scala.js.gen.js.GenEffect

class TestList extends FileDiffSuite {

  trait MapAndFlatMap { this: ListOps with NumericOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] = {
      for {
        x <- xs
        y <- List(unit(1), unit(2), unit(3))
      } yield x * y
    }
  }

  trait Concat { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] =
      xs ++ List(unit(1), unit(2), unit(3))
  }

  trait MkString { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[String] =
      xs.mkString
  }

  trait Prepend { this: ListOps =>
    def test(xs: Rep[List[Int]]): Rep[List[Int]] =
      unit(42) :: xs
  }

  val prefix = "test-out/"

  def testMapAndFlatMap() {
    withOutFile(prefix+"map-flatmap") {
      val prog = new MapAndFlatMap with ListOpsExp with NumericOpsExp
      val codegen = new GenEffect with GenListOps with GenNumericOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "MapAndFlatMap", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"map-flatmap")
  }

  def testConcat() {
    withOutFile(prefix+"concat") {
      val prog = new Concat with ListOpsExp
      val codegen = new GenEffect with GenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "Concat", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"concat")
  }

  def testMkString() {
    withOutFile(prefix+"mkstring") {
      val prog = new MkString with ListOpsExp
      val codegen = new GenEffect with GenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "MkString", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"mkstring")
  }

  def testPrepend() {
    withOutFile(prefix+"prepend") {
      val prog = new Prepend with ListOpsExp
      val codegen = new GenEffect with GenListOps { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "Prepend", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"prepend")
  }

}