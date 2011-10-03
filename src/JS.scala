import scala.virtualization.lms.common._

trait JS extends Arith with Equal with IfThenElse with LiftString with DynamicBase with JSFunctions with JSLiteral

trait JSExp extends JS with ArithExpOpt with EqualExp with IfThenElseExp with DynamicExp with JSFunctionsExp with JSLiteralExp

trait JSGen extends JSGenArith with JSGenEqual with JSGenIfThenElse with JSGenDynamic with JSGenFunctions with JSGenLiteral with JSGenEffect {
  val IR: JSExp
}
