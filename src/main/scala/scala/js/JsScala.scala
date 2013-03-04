package scala.js

import scala.virtualization.lms.common._

/**
 * Trait aggregating several DSLs providing a base language for Web programming with the ability to share code
 * between server and client sides.
 */
trait JsScalaBase extends Base with NumericOps with OrderingOps with Equal with IfThenElse
  with While with BooleanOps with StringOps with Variables with ListOps with ObjectOps
  with TupledFunctions with Structs with PrimitiveOps with MiscOps with TupleOps with ListOps2

trait JsScala extends JsScalaBase with LiftVariables with LiftEquals with LiftNumeric with LiftString with LiftBoolean
  with LiftPrimitives

trait JsScalaExp extends JsScala with EffectExp with NumericOpsExp with OrderingOpsExp with EqualExp
  with IfThenElseExp with WhileExp with BooleanOpsExp with StringOpsExp with VariablesExp with ListOpsExp
  with ObjectOpsExp with TupledFunctionsRecursiveExp with StructExp with PrimitiveOpsExp with MiscOpsExp
  with TupleOpsExp with ListOps2Exp

trait JsScalaExpOpt extends JsScalaExp with NumericOpsExpOpt with EqualExpOpt with IfThenElseExpOpt
  with VariablesExpOpt with ListOpsExpOpt with ObjectOpsExpOpt with StructExpOpt

trait JSGenJsScala extends JSGenEffect with JSGenNumericOps with JSGenOrderingOps with JSGenEqual
  with JSGenIfThenElse with JSGenWhile with JSGenBooleanOps with JSGenStringOps with JSGenVariables
  with JSGenListOps with JSGenObjectOps with JSGenFunctions with JSGenStruct with JSGenPrimitiveOps
  with JSGenMiscOps with JSGenTupleOps with GenericGenUnboxedTupleAccess with JSGenListOps2 {
  val IR: JsScalaExp
}

trait JSGenJsScalaOpt extends JSGenJsScala with JSCodegenOpt

trait ScalaGenJsScala extends ScalaGenEffect with ScalaGenNumericOps with ScalaGenOrderingOps
  with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhile with ScalaGenBooleanOps with ScalaGenStringOps
  with ScalaGenVariables with ScalaGenListOps with ScalaGenObjectOps with ScalaGenFunctions
  with ScalaGenStruct with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenTupleOps with GenericGenUnboxedTupleAccess
  with ScalaGenListOps2 {
  val IR: JsScalaExp
}
