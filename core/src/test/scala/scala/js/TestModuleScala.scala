package scala.js

import java.io.PrintWriter
import scala.js.gen.scala.GenModule
import scala.virtualization.lms.common.{NumericOps, NumericOpsExp, ScalaGenEffect, ScalaGenNumericOps}

class TestModuleScala extends FileDiffSuite {

  trait Prog extends NumericOps {

    def id(x: Rep[Int]) = x

    def first(a: Rep[Int], b: Rep[String]) = a
    
    def add(a: Rep[Int], b: Rep[Int]) = a+b
    
    def third(a: Rep[String], b: Rep[Double], c: Rep[Int]) = {c}
  }

  def testEmitModule() = {
  
    val prog = new Prog with NumericOpsExp 

    val prefix = "test-out/"
    withOutFile(prefix + "module-scala") {
      val genModule = new ScalaGenEffect with ScalaGenNumericOps with GenModule {
        val IR: prog.type = prog
        val module = Module(
            ("pack" -> Module("fun1" -> fun(prog.id _))),
            ("fun2" -> fun(prog.first _)),
            ("fun3" -> fun(prog.add _)),
            ("fun4" -> fun(prog.third _))
        )
      }
      genModule.emitModule("Module 1",new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix + "module-scala")
  }
  
  def testEmitEmptyModule() = {
    val prog = new Prog with NumericOpsExp 

    val prefix = "test-out/"
    withOutFile(prefix + "module-empty-scala") {
      val genModule = new ScalaGenEffect with GenModule {
        val IR: prog.type = prog
        val module = Module()
      }
      genModule.emitModule("Module 1",new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix + "module-empty-scala")
    
  }
}