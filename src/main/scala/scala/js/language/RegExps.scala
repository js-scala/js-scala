package scala.js.language

import scala.util.matching.Regex
import scala.virtualization.lms.common.Base

trait RegExps extends Base {
  def infix_r(r: Rep[String]) = string_regexp(r)
  def infix_test(re: Rep[Regex], str: Rep[String]) = regexp_test(re, str)
  def infix_search(str: Rep[String], re: Rep[Regex]) = string_search(str, re)

  def string_regexp(r: Rep[String]): Rep[Regex]
  def regexp_test(re: Rep[Regex], str: Rep[String]): Rep[Boolean]
  def string_search(str: Rep[String], re: Rep[Regex]): Rep[Int]
}