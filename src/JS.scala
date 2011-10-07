import scala.virtualization.lms.common._

trait JS extends LiftNumeric with NumericOps with Equal with IfThenElse with LiftString with DynamicBase with JSFunctions with JSLiteral

trait JSExp extends JS with NumericOpsExpOpt with EqualExp with IfThenElseExp with DynamicExp with JSFunctionsExp with JSLiteralExp

trait JSGen extends JSGenNumericOps with JSGenEqual with JSGenIfThenElse with JSGenDynamic with JSGenFunctions with JSGenLiteral with JSGenEffect {
  val IR: JSExp
}
