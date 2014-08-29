package scala.js

import java.io.PrintWriter
import scala.js.gen.js.{ GenEffect, GenNumericOps, GenModuleAMD }
import scala.virtualization.lms.common.{PrimitiveOpsExp, PrimitiveOps, NumericOps, NumericOpsExp}

class TestModuleAMD extends FileDiffSuite {

  trait Prog extends NumericOps with PrimitiveOps {

    def id(x: Rep[Int]) = x

    def first(a: Rep[Int], b: Rep[String]) = a

    def add(a: Rep[Int], b: Rep[Int]) = a + b

    def third(a: Rep[String], b: Rep[Double], c: Rep[Int]) = { c }
  }

  def testEmitModule() = {

    val prog = new Prog with NumericOpsExp with PrimitiveOpsExp

    val prefix = "test-out/"
    withOutFile(prefix + "moduleAMD") {
      val genModuleAMD = new GenEffect with GenNumericOps with GenModuleAMD {
        val IR: prog.type = prog
        val module = Module(
          ("pack" -> Module("fun1" -> fun(prog.id _))),
          ("fun2" -> fun(prog.first _)),
          ("fun3" -> fun(prog.add _)),
          ("fun4" -> fun(prog.third _)))
      }
      genModuleAMD.emitModule(new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix + "moduleAMD")
  }

  def testEmitEmptyModule() = {
    val prog = new Prog with NumericOpsExp with PrimitiveOpsExp

    val prefix = "test-out/"
    withOutFile(prefix + "moduleAMD-empty") {
      val genModuleAMD = new GenEffect with GenNumericOps with GenModuleAMD {
        val IR: prog.type = prog
        val module = Module()
      }
      genModuleAMD.emitModule(new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix + "moduleAMD-empty")
  }
}