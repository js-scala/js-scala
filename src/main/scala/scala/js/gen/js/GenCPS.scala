package scala.js.gen.js

import scala.js.exp.CPSExp

trait GenCPS extends GenProxy {
  val IR: CPSExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case CellNode() => emitValDef(sym, "new Cell()")
    case _ => super.emitNode(sym, rhs)
  }
}