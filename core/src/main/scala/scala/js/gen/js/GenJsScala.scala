package scala.js.gen.js

import scala.js.exp.JsScalaExp
import scala.virtualization.lms.common.GenericGenUnboxedTupleAccess

trait GenJsScala extends GenEffect with GenNumericOps with GenOrderingOps with GenEqual
  with GenIfThenElse with GenWhile with GenBooleanOps with GenStringOps with GenVariables
  with GenListOps with GenObjectOps with GenFunctions with GenStruct with GenPrimitiveOps
  with GenMiscOps with GenTupleOps with GenericGenUnboxedTupleAccess with GenListOps2 {
  val IR: JsScalaExp
}

trait GenJsScalaOpt extends GenJsScala with CodegenOpt