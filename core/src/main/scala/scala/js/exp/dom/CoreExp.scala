package scala.js.exp.dom

import scala.js.language.dom.Core
import scala.virtualization.lms.common.EffectExp

trait CoreExp extends Core with EffectExp {

  // TODO Check that tokens are well formed
  // TODO Handle effects in contains
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]) = DOMTokenListContains(ts, token)
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListAdd(ts, tokens))
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListRemove(ts, tokens))

  case class DOMTokenListContains(ts: Exp[DOMTokenList], token: Exp[String]) extends Def[Boolean]
  case class DOMTokenListAdd(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]
  case class DOMTokenListRemove(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]
}

