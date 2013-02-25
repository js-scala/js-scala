package scala.js

import scala.virtualization.lms.common._

/**
 * Extends the JsScala language with some client-side only DSLs
 */
trait JSBase extends JsScalaBase with DynamicBase with Arrays with JSRegExps with OptionOps

trait JS extends JSBase with JsScala

trait JSExp extends JS with JsScalaExp with DynamicExp with ArraysExp
  with JSRegExpsExp with OptionOpsExp

trait JSExpOpt extends JSExp with JsScalaExpOpt with NumericOpsExpOpt

trait JSGen extends JSGenJsScala with JSGenDynamic with JSGenArrays
  with JSGenRegExps with JSGenTupleOps with GenericGenUnboxedTupleAccess with JSGenOptionOps {
  val IR: JSExp
}

trait JSGenOpt extends JSGen with JSCodegenOpt
