package scala.js.language

import scala.virtualization.lms.common._

/**
 * Trait aggregating several DSLs providing a base language for Web programming with the ability to share code
 * between server and client sides.
 *
 * Integrates most of the language units defined by LMS.
 */
trait JsScalaBase extends Base with NumericOps with OrderingOps with Equal with IfThenElse
  with While with BooleanOps with StringOps with Variables with ListOps with ObjectOps
  with TupledFunctions with Structs with PrimitiveOps with MiscOps with TupleOps with ListOps2

/**
 * Same as [[scala.js.language.JsScala]] but with implicit conversions automatically lifting values to `Rep` values when needed.
 */
trait JsScala extends JsScalaBase with LiftVariables with LiftEquals with LiftNumeric with LiftString with LiftBoolean
  with LiftPrimitives