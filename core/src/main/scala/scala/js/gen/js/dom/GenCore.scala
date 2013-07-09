package scala.js.gen.js.dom

import scala.js.exp.dom.CoreExp
import scala.js.gen.js.GenEffect

trait GenCore extends GenEffect {
  val IR: CoreExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DOMTokenListContains(ts, token) =>
      emitValDef(sym, quote(ts) + ".contains(" + quote(token) + ")")
    case DOMTokenListAdd(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".add(" + tokens.map(quote).mkString(", ") + ")")
    case DOMTokenListRemove(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".remove(" + tokens.map(quote).mkString(", ") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}