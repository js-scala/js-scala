package scala.js

import scala.language.experimental.macros
import scala.virtualization.lms.common._
import java.io.PrintWriter
import gen.js.{GenEffect, GenDebug, GenEqual, GenRecords}
import language.{Debug, Records}
import exp.{DebugExp, RecordsExp}
import macroimpl.Record

class TestRecord extends FileDiffSuite {

  def testRecord() {

    val prefix = "test-out/"

    trait DSL extends Base with Records with Debug with Equal
    trait DSLExp extends DSL with RecordsExp with DebugExp with EqualExp
    trait DSLJSGen extends GenEffect with GenRecords with GenDebug with GenEqual { val IR: DSLExp }

    trait Prog extends DSL {
      class User(name: String, age: Int, mail: String, lePoint: Point) extends Record
      implicit def userOps(u: Rep[User]) = recordOps(u)

      class Point(x: Int, y: Int, dimension: Dimension) extends Record
      implicit def pointOps(p:Rep[Point]) = recordOps(p)

      class Dimension(nom: String, power: List[String]) extends Record
      implicit def dimensionOps(d:Rep[Dimension]) = recordOps(d)

      def main(n: Rep[String]) = {

        val Dimension = record[Dimension]
        val dim = Dimension(unit("5D"), unit(List("jour", "nuit")))
        
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
        /*log(u === v)
        
        val z = v.age 
        log(z)*/
        
        //val q = p.copy()
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
 