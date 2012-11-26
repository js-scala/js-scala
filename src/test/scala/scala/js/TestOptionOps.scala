package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

class TestOptionOps extends FileDiffSuite {
  val prefix = "test-out/"

  trait DSL extends Base with OptionOps with NumericOps
  trait DSLExp extends DSL with OptionOpsExp with NumericOpsExp
  trait DSLJSGen extends JSGenEffect with JSGenOptionOps with JSGenNumericOps { val IR: DSLExp }

  def testOption() {

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

}