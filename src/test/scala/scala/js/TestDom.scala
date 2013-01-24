package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

class TestDom extends FileDiffSuite {
  val prefix = "test-out/"

  trait DSL extends Base with JSDom with JSDebug
  trait DSLExp extends DSL with JSDomExp with JSDebugExp
  trait DSLJSGen extends JSGenEffect with JSGenDom with JSGenDebug { val IR: DSLExp }

  def testOn() {

    trait Prog { this: DSL =>

      def main(): Rep[Unit] = {
        window.on(MouseUp) { e =>
          log(e)
        }
      }
    }
    withOutFile(prefix+"dom") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource0(prog.main _, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"dom")
  }
}