package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import scala.js.exp.JSLiteralExp
import scala.js.gen.js.GenJSLiteral
import scala.js.gen.js.NestedCodegen
import scala.js.gen.js.GenStruct

trait PersonDSL extends Base {
  type Person
  def newPerson(name: Rep[String]): Rep[Person]
  def infix_name(person: Rep[Person]): Rep[String]
}

trait PersonExp extends PersonDSL with BaseExp { this: StructExp =>
  override type Person = Map[String, Any]
  override def newPerson(name: Exp[String]) = struct(ClassTag[Person]("Person"), Map("name"->name))
  override def infix_name(person: Rep[Person]) = field[String](person, "name")
}

trait StructProg { this: PersonDSL =>
  def test(name: Rep[String]): Rep[String] = {
    val person = newPerson(name)
    person.name
  }
}

class TestStruct extends FileDiffSuite {
  val prefix = "test-out/"

  def testStruct() {
    withOutFile(prefix+"struct") {
      val prog = new StructProg with PersonExp with StructExp with JSLiteralExp
      val codegen = new NestedCodegen with GenStruct with GenJSLiteral { val IR: prog.type = prog }
      codegen.emitSource(prog.test, "test", new PrintWriter(System.out))
    }
    assertFileEqualsCheck(prefix+"struct")
  }

}