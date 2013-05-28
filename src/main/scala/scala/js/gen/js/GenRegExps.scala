package scala.js.gen.js

import scala.js.exp.RegExpsExp

trait GenRegExps extends GenBase {
  val IR: RegExpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringRegExp(r) => emitValDef(sym, "new RegExp(" + quote(r) + ")")
    case RegExpTest(re, str) => emitValDef(sym, quote(re) + ".test(" + quote(str) + ")")
    case StringSearch(str, re) => emitValDef(sym, quote(str) + ".search(" + quote(re) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}