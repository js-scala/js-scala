package scala.js.exp

import scala.virtualization.lms.common._
import scala.js.language.JsScala

trait JsScalaExp extends JsScala with EffectExp with NumericOpsExp with OrderingOpsExp with EqualExp
  with IfThenElseExp with WhileExp with BooleanOpsExp with StringOpsExp with VariablesExp with ListOpsExp
  with ObjectOpsExp with TupledFunctionsRecursiveExp with StructExp with PrimitiveOpsExp with MiscOpsExp
  with TupleOpsExp with ListOps2Exp

trait JsScalaExpOpt extends JsScalaExp with NumericOpsExpOpt with EqualExpOpt with IfThenElseExpOpt
  with VariablesExpOpt with ListOpsExpOpt with ObjectOpsExpOpt with StructExpOpt