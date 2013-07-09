package scala.js.exp.dom

import scala.js.exp.OptionOpsExp
import scala.js.language.dom.ElementOps
import scala.virtualization.lms.common.{EffectExp, IfThenElseExp, TupledFunctionsExp}

trait ElementOpsExp extends ElementOps with EffectExp with TupledFunctionsExp with OptionOpsExp with IfThenElseExp {

  def css_fontweight(css: Rep[CSSStyleDeclaration]) = reflectEffect(CSSFontWeight(css))
  def css_set_fontweight(css: Rep[CSSStyleDeclaration], v: Rep[Int]) = reflectEffect(CSSSetFontWeight(css, v))

  def element_setAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) = reflectEffect(ElementSetAttribute(e, name, value))
  def element_tagName(e: Exp[Element]) = ElementTagName(e)
  def element_className(e: Exp[Element]) = ElementClassName(e)
  def element_parentNode(e: Exp[Element]) = ElementParentNode(e)
  def element_previousSibling(e: Exp[Element]) = ElementPreviousSibling(e)
  def element_classList(e: Exp[Element]) = ElementClassList(e)
  def element_removeChild(e: Exp[Element], c: Exp[Element]) = reflectEffect(ElementRemoveChild(e, c))
  def element_appendChild(e: Exp[Element], c: Exp[Element]) = reflectEffect(ElementAppendChild(e, c))
  def element_focus(e: Exp[Element]) = reflectEffect(ElementFocus(e))
  def element_closest_reified: Exp[((Element, Element => Boolean)) => Option[Element]] =
    fun { (e: Exp[Element], p: Exp[Element => Boolean]) =>
      for {
        parent <- e.parentNode
        closest <- if (p(parent)) some(parent) else element_closest_reified(parent, p)
      } yield closest
    }
  def element_closest(e: Exp[Element], p: Exp[Element] => Exp[Boolean]) = element_closest_reified(e, p)
  def element_prev_reified: Exp[((Element, Element => Boolean)) => Option[Element]] =
    fun { (e: Exp[Element], p: Exp[Element => Boolean]) =>
      for {
        sib <- e.previousSibling
        prev <- if (p(sib)) some(sib) else element_prev_reified(sib, p)
      } yield prev
    }
  def element_prev(e: Exp[Element], p: Exp[Element] => Exp[Boolean]) = element_prev_reified(e, p)
  def element_remove(e: Exp[Element]) = fun { e: Exp[Element] =>
    e.parentNode.fold((), _.removeChild(e))
  } apply e
  def element_style(e: Exp[Element]) = ElementStyle(e)
  
  def input_set_disabled(input: Exp[Input], value: Exp[Boolean]) = reflectEffect(InputSetDisabled(input, value))
  def input_disabled(input: Exp[Input]) = reflectEffect(InputDisabled(input)) // TODO Optimize effect description
  def input_set_name(input: Exp[Input], value: Exp[String]) = reflectEffect(InputSetName(input, value))
  def input_name(input: Exp[Input]) = reflectEffect(InputName(input))
  def input_value(input: Exp[Input]) = reflectEffect(InputValue(input))

  def form_submit(form: Exp[Form]) = reflectEffect(FormSubmit(form))


  case class CSSFontWeight(css: Exp[CSSStyleDeclaration]) extends Def[Int]
  case class CSSSetFontWeight(css: Exp[CSSStyleDeclaration], v: Exp[Int]) extends Def[Unit]

  case class ElementSetAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) extends Def[Unit]
  case class ElementTagName(e: Exp[Element]) extends Def[String]
  case class ElementClassName(e: Exp[Element]) extends Def[String]
  case class ElementParentNode(e: Exp[Element]) extends Def[Option[Element]]
  case class ElementPreviousSibling(e: Exp[Element]) extends Def[Option[Element]]
  case class ElementClassList(e: Exp[Element]) extends Def[DOMTokenList]
  case class ElementRemoveChild(e: Exp[Element], c: Exp[Element]) extends Def[Unit]
  case class ElementAppendChild(e: Exp[Element], c: Exp[Element]) extends Def[Unit]
  case class ElementFocus(e: Exp[Element]) extends Def[Unit]
  case class ElementStyle(e: Exp[Element]) extends Def[CSSStyleDeclaration]
  
  case class InputSetDisabled(input: Exp[Input], value: Exp[Boolean]) extends Def[Unit]
  case class InputDisabled(input: Exp[Input]) extends Def[Boolean]
  case class InputSetName(input: Exp[Input], value: Exp[String]) extends Def[Unit]
  case class InputName(input: Exp[Input]) extends Def[String]
  case class InputValue(input: Exp[Input]) extends Def[String]

  case class FormSubmit(form: Exp[Form]) extends Def[Unit]

}