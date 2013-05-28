package scala.js.gen.js

import scala.js.exp.AjaxExp

trait GenAjax extends GenBase {
  val IR: AjaxExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case AjaxGet(req, succ) => 
      stream.println(quote(req) + ".success = " + quote(succ)) 
      emitValDef(sym, "$.ajax(" + quote(req) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}