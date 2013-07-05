package scala.js.gen.js

import scala.js.exp.DebugExp

trait GenDebug extends GenEffect {
  val IR: DebugExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Log(s) => emitValDef(sym, "console.log(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}