package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

class TestPolymorphism extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testPolymorphism = {
    trait Prog { this: Base with StringOps with ObjectOps with JSDebug =>

      trait Show[A] {
        def show(a: Rep[A]): Rep[String]
      }

      implicit val showString = new Show[String] {
        def show(s: Rep[String]) = "\"" + s + "\""
      }

      implicit val showInt = new Show[Int] {
        def show(x: Rep[Int]) = x.toString()
      }

      // Polymorphic function
      def show[A](a: Rep[A])(implicit show: Show[A]) = show.show(a)

      def main(x: Rep[Int], s: Rep[String]): Rep[Unit] = {
        log(show(x))
        log(show(s))
      }
    }
    
    withOutFile(prefix+"polymorphism") {
      val prog = new Prog with BaseExp with StringOpsExp with ObjectOpsExp with JSDebugExp
      val codegen = new JSNestedCodegen with JSGenStringOps with JSGenObjectOps with JSGenDebug { val IR: prog.type = prog }
      codegen.emitSource2(prog.main, "main", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"polymorphism")
  }
}
