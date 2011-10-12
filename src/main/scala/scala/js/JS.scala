package scala.js

import scala.virtualization.lms.common._

trait JS extends LiftNumeric with NumericOps with Equal with IfThenElse with LiftString with DynamicBase with Arrays with LiftVariables with Variables with JSFunctions with JSLiteral

trait JSExp extends JS with NumericOpsExpOpt with EqualExp with IfThenElseExp with DynamicExp with ArraysExp with VariablesExp with JSFunctionsExp with JSLiteralExp

trait JSGen extends JSGenNumericOps with JSGenEqual with JSGenIfThenElse with JSGenDynamic with JSGenArrays with JSGenVariables with JSGenFunctions with JSGenLiteral with JSGenTupleOps with GenericGenUnboxedTupleAccess {
  val IR: JSExp
}
