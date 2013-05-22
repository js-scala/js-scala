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
        val tag = document.findAll(unit("babar"))
        log(tag: Rep[List[Element]])
        val classe = document.findAll(unit(".babar"))
        log(classe: Rep[List[Element]])
        val tag2 = document.findAll(unit("foo #bar baz"))
        log(tag2: Rep[List[Element]])
        val tag3 = document.findAll(unit("foo .bar baz"))
        log(tag3: Rep[List[Element]])
        val id = document.find(unit(" #foo-1-0    "))
        log(id: Rep[Option[Element]])
        val error = document.find(unit(" #---    "))
        log(error: Rep[Option[Element]])
        val id2 = document.find(unit(" #-a_a    "))
        log(id2: Rep[Option[Element]])
        val id3 = document.find(unit(" #bbaa    "))
        log(id3: Rep[Option[Element]])
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