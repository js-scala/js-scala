package scala.js.exp

import scala.js.language.RegExps
import scala.util.matching.Regex
import scala.virtualization.lms.common.BaseExp

trait RegExpsExp extends RegExps with BaseExp {
  case class StringRegExp(r: Exp[String]) extends Def[Regex]
  case class RegExpTest(re: Exp[Regex], str: Exp[String]) extends Def[Boolean]
  case class StringSearch(str: Exp[String], re: Exp[Regex]) extends Def[Int]

  override def string_regexp(r: Rep[String]): Rep[Regex] = StringRegExp(r)
  override def regexp_test(re: Rep[Regex], str: Rep[String]): Rep[Boolean] = RegExpTest(re, str)
  override def string_search(str: Rep[String], re: Rep[Regex]): Rep[Int] = StringSearch(str, re)
}