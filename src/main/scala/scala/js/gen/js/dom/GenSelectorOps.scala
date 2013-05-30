package scala.js.gen.js.dom

import scala.js.exp.dom.SelectorOpsExp
import scala.js.gen.js.GenEffect

trait GenSelectorOps extends GenEffect {
  val IR: SelectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SelectorFind(s, selector) =>
      emitValDef(sym, quote(s) + ".querySelector(" + quote(selector) + ")")
    case SelectorGetElementById(s, selector) =>
      emitValDef(sym, quote(s) + ".getElementById(" + quote(selector) + ")")
    case SelectorFindAll(s, selector) =>
      emitValDef(sym, quote(s) + ".querySelectorAll(" + quote(selector) + ")")
    case SelectorGetElementsByClassName(s, selector) =>
      emitValDef(sym, quote(s) + ".getElementsByClassName(" + quote(selector) + ")")
    case SelectorGetElementsByTagName(s, selector) =>
      emitValDef(sym, quote(s) + ".getElementsByTagName(" + quote(selector) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}