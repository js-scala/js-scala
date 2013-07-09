package scala.js.gen.js.dom

import scala.js.exp.dom.BrowserExp
import scala.js.gen.js.GenEffect

trait GenBrowser extends GenEffect with GenSelectorOps with GenEventOps with GenElementOps  {
  val IR: BrowserExp
  import IR._

  override def quote(x: Exp[Any]) = x match {
    case `window` => "window"
    case WindowDocument => "document"
    case WindowHistory => "history"
    case _ => super.quote(x)
  }
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HistoryReplaceState(h, state, title, url) =>
      emitValDef(sym, quote(h) + ".replaceState(" + quote(state) + ", " + quote(title) + ", " + quote(url) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}