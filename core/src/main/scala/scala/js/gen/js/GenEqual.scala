package scala.js.gen.js

import scala.virtualization.lms.common.EqualExp

trait GenEqual extends GenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case NotEqual(a,b) =>  emitValDef(sym, "" + quote(a) + "!=" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
