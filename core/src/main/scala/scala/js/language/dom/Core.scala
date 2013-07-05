package scala.js.language.dom

import scala.virtualization.lms.common.Base

trait Core extends Base {

  trait DOMTokenList
  implicit class DOMTokenListOps(ts: Rep[DOMTokenList]) {
    def contains(token: Rep[String]) = domtokenlist_contains(ts, token)
    def add(tokens: Rep[String]*) = domtokenlist_add(ts, tokens)
    def remove(tokens: Rep[String]*) = domtokenlist_remove(ts, tokens)
  }
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]): Rep[Boolean]
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]

}