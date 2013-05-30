package scala.js.gen.js

import scala.js.exp.StateOpsExp

trait GenStateOps extends GenEffect with GenVariables with GenTupleOps {
  val IR: StateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StateBlock(b) =>
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}