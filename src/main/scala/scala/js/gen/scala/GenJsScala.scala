package scala.js.gen.scala

import scala.virtualization.lms.common._
import scala.js.exp.JsScalaExp

trait GenJsScala extends ScalaGenEffect with ScalaGenNumericOps with ScalaGenOrderingOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenBooleanOps with ScalaGenStringOps
  with ScalaGenVariables with ScalaGenListOps with ScalaGenObjectOps with ScalaGenTupledFunctions
  with ScalaGenStruct with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps with GenericGenUnboxedTupleAccess
  with GenListOps2 {
  val IR: JsScalaExp
}