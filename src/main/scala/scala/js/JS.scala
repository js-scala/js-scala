package scala.js

import scala.virtualization.lms.common._

trait JS extends LiftNumeric with NumericOps with Equal with IfThenElse with LiftString with DynamicBase with Arrays with JSFunctions with JSLiteral

trait JSExp extends JS with NumericOpsExpOpt with EqualExp with IfThenElseExp with DynamicExp with ArraysExp with JSFunctionsExp with JSLiteralExp

trait JSGen extends JSGenNumericOps with JSGenEqual with JSGenIfThenElse with JSGenDynamic with JSGenArrays with JSGenFunctions with JSGenLiteral {
  val IR: JSExp
}
