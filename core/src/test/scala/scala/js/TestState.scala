package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.js.language.{StateOps, Debug}
import scala.js.exp.{StateOpsExp, DebugExp}

class TestState extends FileDiffSuite {
  val prefix = "test-out/"

  trait DSL extends Base with StateOps with NumericOps with LiftNumeric with Debug with PrimitiveOps
  trait DSLExp extends DSL with StateOpsExp with VariablesExp with TupleOpsExp with NumericOpsExp with DebugExp with PrimitiveOpsExp
  trait DSLGen extends gen.js.GenVariables with gen.js.GenTupleOps with gen.js.GenStateOps with gen.js.GenNumericOps with gen.js.GenDebug { val IR: DSLExp }

  def testModify() {

    trait Prog { this: DSL =>

      def main(init: Rep[Int]): Rep[Int] = {
        import State._

        val p = for {
          _ <- modify[Int](_ + 10)
          _ <- modify[Int](_ + 20)
        } yield ()

        p.exec(init)
      }
    }
    withOutFile(prefix+"state-modify") {
      val prog = new Prog with DSLExp
      val codegen = new DSLGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"state-modify")
  }

  def testPutAndGet() {
    trait Prog { this: DSL =>
      import State._
      
      def main(init: Rep[Int]): Rep[Int] = {
        val inc = for {
          x <- get[Int]
          _ <- put(x + 1)
        } yield x

        inc.eval(init)
      }

      def putPut(init: Rep[Int]): Rep[Int] = {
        val p = for {
          _ <- put(42)
          _ <- put(0)
        } yield ()
        p.exec(init)
      }
    }
    withOutFile(prefix+"state-put-get") {
      val prog = new Prog with DSLExp
      val codegen = new DSLGen { val IR: prog.type = prog }
      val out = new PrintWriter(System.out)

      println("// TODO Still too much aliases")
      println("// TODO The assignment should be discarded")
      codegen.emitSource(prog.main, "test", out)

      println("// TODO Only the last put should remain")
      codegen.emitSource(prog.putPut, "putPut", out)
    }
    assertFileEqualsCheck(prefix+"state-put-get")
  }

  def testEvalExec() {
    trait Prog { this: DSL =>
      import State._

      val noState = for {
        x <- state[Int](41)
        y <- state[Int](1)
      } yield x + y

      val applyEffect = for {
        _ <- put(42)
      } yield 0
    }
    withOutFile(prefix+"state-eval-exec") {
      val prog = new Prog with DSLExp
      val codegen = new DSLGen { val IR: prog.type = prog }
      val out = new PrintWriter(System.out)

      import prog.StateMToOps
      println("// TODO Still too much aliases")
      codegen.emitSource(prog.noState.eval, "noState_eval", out)

      codegen.emitSource(prog.noState.exec, "noState_exec", out)

      codegen.emitSource(prog.applyEffect.eval, "applyEffect_eval", out)

      codegen.emitSource(prog.applyEffect.exec, "applyEffect_exec", out)

      codegen.emitSource(prog.applyEffect.run, "applyEffect_run", out)
    }
    assertFileEqualsCheck(prefix+"state-eval-exec")
  }

  def testHeterogeneousTypes() {
    import scala.js.gen.scala.GenStateOps

    trait Prog { this: Base with StateOps with StringOps =>
      import State._
      val p = for {
        s <- get[String]
        _ <- put(s + "0")
      } yield s.toInt
      def main(zero: Rep[String]): Rep[Int] = p.eval(zero)
    }
    withOutFile(prefix+"state-heterogeneous-types") {
      val prog = new Prog with StateOpsExp with VariablesExp with TupleOpsExp with StringOpsExp
      val codegen = new GenStateOps with ScalaGenVariables with ScalaGenTupleOps with ScalaGenStringOps { val IR: prog.type = prog }
      codegen.emitSource(prog.main, "main", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"state-heterogeneous-types")
  }

}