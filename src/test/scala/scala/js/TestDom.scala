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

      def main() = {
        window.on(MouseUp) { e =>
          log(e)
        }
      }

      def selectors() = {
        val bar = document.find(unit(".bar"))
        log(bar: Rep[Option[Element]])
        val input = document.find[Input](unit("input"))
        log(input: Rep[Option[Input]])
      }
    }
    withOutFile(prefix+"dom") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource0(prog.main _, "main", new PrintWriter(System.out))
      codegen.emitSource0(prog.selectors _, "selectors", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"dom")
  }
}