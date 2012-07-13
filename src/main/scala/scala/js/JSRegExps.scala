package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

import scala.util.matching.Regex

trait JSRegExps extends Base {
  def infix_r(r: Rep[String]) = string_regexp(r)
  def infix_test(re: Rep[Regex], str: Rep[String]) = regexp_test(re, str)
  def infix_search(str: Rep[String], re: Rep[Regex]) = string_search(str, re)

  def string_regexp(r: Rep[String]): Rep[Regex]
  def regexp_test(re: Rep[Regex], str: Rep[String]): Rep[Boolean]
  def string_search(str: Rep[String], re: Rep[Regex]): Rep[Int]
}

trait JSRegExpsExp extends JSRegExps with BaseExp {
  case class StringRegExp(r: Exp[String]) extends Def[Regex]
  case class RegExpTest(re: Exp[Regex], str: Exp[String]) extends Def[Boolean]
  case class StringSearch(str: Exp[String], re: Exp[Regex]) extends Def[Int]

  override def string_regexp(r: Rep[String]): Rep[Regex] = StringRegExp(r)
  override def regexp_test(re: Rep[Regex], str: Rep[String]): Rep[Boolean] = RegExpTest(re, str)
  override def string_search(str: Rep[String], re: Rep[Regex]): Rep[Int] = StringSearch(str, re)
}

trait JSGenRegExps extends JSGenBase {
  val IR: JSRegExpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringRegExp(r) => emitValDef(sym, "new RegExp(" + quote(r) + ")")
    case RegExpTest(re, str) => emitValDef(sym, quote(re) + ".test(" + quote(str) + ")")
    case StringSearch(str, re) => emitValDef(sym, quote(str) + ".search(" + quote(re) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
