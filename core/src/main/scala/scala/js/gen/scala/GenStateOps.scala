package scala.js.gen.scala

import scala.js.exp.StateOpsExp
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.common.ScalaGenEffect
import scala.virtualization.lms.common.ScalaGenVariables

trait GenStateOps extends ScalaGenEffect with ScalaGenVariables {
  val IR: StateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StateBlock(b) =>
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}