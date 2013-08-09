package scala.js

import scala.language.experimental.macros
import scala.virtualization.lms.common._
import java.io.PrintWriter
import gen.js.{GenEffect, GenDebug, GenRecords, GenListOps}
import language.{Debug, Records}
import exp.{DebugExp, RecordsExp}

class TestRecord extends FileDiffSuite {

  def testRecord() {

    val prefix = "test-out/"

    trait DSL extends Base with Records with ListOps with Debug
    trait DSLExp extends DSL with RecordsExp with ListOpsExp with DebugExp
    trait DSLJSGen extends GenEffect with GenRecords with GenListOps with GenDebug { val IR: DSLExp }

    trait Prog extends DSL {
      case class User(name: String, age: Int, mail: String, lePoint: Point) extends Record
      implicit def userOps(u: Rep[User]) = recordOps(u)

      case class Point(x: Int, y: Int, dimension: Dimension) extends Record
      implicit def pointOps(p:Rep[Point]) = recordOps(p)

      case class Dimension(nom: String, power: List[String]) extends Record
      implicit def dimensionOps(d:Rep[Dimension]) = recordOps(d)

      def main(n: Rep[String]) = {

        val Dimension = record[Dimension]
        val dim = Dimension(unit("5D"), List(unit("jour"), unit("nuit")))
        
        val Point = record[Point]
        val p = Point(unit(15), unit(22), dim)
        
        val User = record[User]
        val u = User(unit("Zev"), unit(22), unit("plop"), p)
        log(u)
        log(u.name)
        
        val w = u.copy(u.name, age=unit(33), u.mail, p)
        log(w)
        log(w.age)

        log(u === w)
       }
     }

     withOutFile(prefix+"record") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main _, "main", new PrintWriter(System.out))
     }
     assertFileEqualsCheck(prefix+"record")
     
  }
}
 