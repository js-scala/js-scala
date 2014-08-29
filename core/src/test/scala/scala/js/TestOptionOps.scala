package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import language.OptionOps
import exp.OptionOpsExp
import gen.js.{GenEffect, GenNumericOps, GenOptionOps, GenEqual, GenIfThenElse, GenPrimitiveOps}

class TestOptionOps extends FileDiffSuite {
  val prefix = "test-out/"

  trait DSL extends Base with OptionOps with NumericOps with PrimitiveOps
  trait DSLExp extends DSL with OptionOpsExp with NumericOpsExp with PrimitiveOpsExp
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
  
  def testGetOrElse() {
    trait Prog { this: DSL =>
      def main(maybeX: Rep[Option[Int]]): Rep[Int] = maybeX getOrElse unit(0)
    }
    withOutFile(prefix+"option-getorelse") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"option-getorelse")
  }

  def testDependencies() {
    trait Prog extends DSL with TupledFunctions with IfThenElse {
      lazy val rec: Rep[((Int => Option[Int], Int => Boolean, Int)) => Option[Int]] = fun { (f: Rep[Int => Option[Int]], p: Rep[Int => Boolean], x: Rep[Int]) =>
        for {
          y <- f(x)
          z <- if (p(y)) some(y) else rec(f, p, y)
        } yield z
        //if (p(x)) some(x) else rec(f, p, x)
      }
      def main(f: Rep[Int => Option[Int]], p: Rep[Int => Boolean], x: Rep[Int]) = rec(f, p, x)
    }
    withOutFile(prefix + "option-deps") {
      val prog = new Prog with DSLExp with TupledFunctionsRecursiveExp with IfThenElseExp
      val codegen = new DSLJSGen with gen.js.GenFunctions with GenericGenUnboxedTupleAccess with GenIfThenElse { val IR: prog.type = prog }
      codegen.emitSource3(prog.main, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix + "option-deps")
  }
}