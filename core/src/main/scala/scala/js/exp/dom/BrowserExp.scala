package scala.js.exp.dom

import scala.js.language.dom.Browser
import scala.virtualization.lms.common.EffectExp

trait BrowserExp extends Browser with EffectExp with SelectorOpsExp with EventOpsExp with ElementOpsExp {
  
  case object window extends Exp[Window]
  def infix_document(w: Exp[Window]) = WindowDocument
  def infix_history(w: Exp[Window]) = WindowHistory
  
  def history_replaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) = reflectEffect(HistoryReplaceState(h, state, title, url))


  case object WindowDocument extends Exp[Document]
  case object WindowHistory extends Exp[History]
  
  case class HistoryReplaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) extends Def[Unit]

}