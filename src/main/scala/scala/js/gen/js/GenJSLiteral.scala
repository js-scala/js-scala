package scala.js.gen.js

import scala.js.exp.JSLiteralExp
 
trait GenJSLiteral extends GenBase {
  val IR: JSLiteralExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case JSLiteralDef(members) => emitValDef(sym, literalObjectDef(members))
    case MemberSelect(receiver, field) =>
      emitValDef(sym, literalObjectSelect(receiver, field))
    case _ => super.emitNode(sym, rhs)
  }
}