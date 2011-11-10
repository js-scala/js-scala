package scala.js

import scala.virtualization.lms.common._

trait JSK extends LiftNumeric with LiftString with NumericOps with Equal with IfThenElse with JSFunctions

trait JSKExp extends JSK with NumericOpsExpOpt with EqualExp with IfThenElseExp with JSFunctionsExp

trait JSKGen extends JSGenNumericOps with JSGenEqual with JSGenIfThenElse with JSGenFunctions with JSGenTupleOps with GenericGenUnboxedTupleAccess {
  val IR: JSKExp
}

trait JSKGenOpt extends JSKGen with JSCodegenOpt

trait JS extends LiftNumeric with NumericOps with OrderingOps with Equal with IfThenElse with While with LiftBoolean with BooleanOps with LiftString with StringOps with DynamicBase with Arrays with Variables with JSFunctions with JSLiteral

trait JSExp extends JS with NumericOpsExpOpt with OrderingOpsExp with EqualExp with IfThenElseExp with WhileExp with BooleanOpsExp with StringOpsExp with DynamicExp with ArraysExp with VariablesExp with JSFunctionsExp with JSLiteralExp

trait JSGen extends JSGenNumericOps with JSGenOrderingOps with JSGenEqual with JSGenIfThenElse with JSGenWhile with JSGenBooleanOps with JSGenStringOps with JSGenDynamic with JSGenArrays with JSGenVariables with JSGenFunctions with JSGenLiteral with JSGenTupleOps with GenericGenUnboxedTupleAccess {
  val IR: JSExp
}

trait JSGenOpt extends JSGen with JSCodegenOpt
