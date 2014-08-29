package scala.js.gen.js

import scala.js.exp.ProxyExp

trait GenProxy extends GenEffect {
  val IR: ProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MethodCall(receiver, method, args) =>  emitValDef(sym,
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case SuperMethodCall(receiver, parentConstructor, method, args) =>  emitValDef(sym,
      (parentConstructor match { case Some(parentConstructor) => quote(parentConstructor); case None => "Object" }) +
      ".prototype." + method + ".call" + (receiver::args).map(quote).mkString("(", ",", ")"))
    case FieldAccess(receiver, field) =>  emitValDef(sym,
      quote(receiver) + "." + field)
    case ProxyFieldUpdate(receiver, field, value) =>  emitValDef(sym,
      quote(receiver) + "." + field + " = " + quote(value))
    case _ => super.emitNode(sym, rhs)
  }
}