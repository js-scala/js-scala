package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import language.OptionOps
import exp.OptionOpsExp
import gen.js.{GenEffect, GenNumericOps, GenOptionOps}

class TestOptionOps extends FileDiffSuite {
  val prefix = "test-out/"

  trait DSL extends Base with OptionOps with NumericOps
  trait DSLExp extends DSL with OptionOpsExp with NumericOpsExp
  trait DSLJSGen extends GenEffect with GenOptionOps with GenNumericOps { val IR: DSLExp }

  def testMapFlatMap() {

    trait Prog { this: DSL =>

      def main(maybeX: Rep[Option[Int]], maybeY: Rep[Option[Int]]): Rep[Option[Int]] =
        for {
          x <- maybeX
          y <- maybeY
        } yield x + y
    }
    withOutFile(prefix+"option") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource2(prog.main, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"option")
  }

  def testFold() {
    trait Prog { this: DSL =>
      def main(maybeX: Rep[Option[Int]]): Rep[Int] = maybeX.fold(
        unit(0),
        x => x + unit(1)
      )
    }
    withOutFile(prefix+"option-fold") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"option-fold")
  }

}