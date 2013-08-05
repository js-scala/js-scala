package scala.js.language.dom

import scala.virtualization.lms.common.Base

trait Browser extends Base with SelectorOps with EventOps with ElementOps {

  trait Window extends EventTarget
  val window: Rep[Window]
  def infix_document(w: Rep[Window]): Rep[Document]
  def infix_history(w: Rep[Window]): Rep[History]

  // Convenient aliases
  val document = window.document
  val history = window.history

  trait Document extends Selector with EventTarget
  implicit class DocumentOps(document: Rep[Document]) {
    def body = document_body(document)
  }
  def document_body(document: Rep[Document]): Rep[Element]

  trait History
  implicit class HistoryOps(h: Rep[History]) {
    def replaceState(state: Rep[_], title: Rep[String], url: Rep[String]) = history_replaceState(h, state, title, url)
  }
  def history_replaceState(h: Rep[History], state: Rep[_], title: Rep[String], url: Rep[String]): Rep[Unit]

}