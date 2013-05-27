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
      
      def testFind(el: Rep[Element]) = {
    	 el.find(unit("#some-id"))
    	 document.find(unit("#some-id"))
      }


      def selectors() = {
        document.findAll(unit("babar"))
        document.findAll(unit(".babar"))
        document.findAll(unit("foo #bar baz"))
        document.findAll(unit("foo .bar baz"))
        document.find(unit(" #foo-1-0    "))
        document.find(unit(" #---    "))
        document.find(unit(" #-a_a    "))
        document.find(unit(" #bbaa    "))
        document.find[Input](unit("input"))
      }
    }
    withOutFile(prefix+"dom") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource0(prog.main _, "main", new PrintWriter(System.out))
      codegen.emitSource0(prog.selectors _, "selectors", new PrintWriter(System.out))
      codegen.emitSource(prog.testFind _, "selectors", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"dom")
  }
}