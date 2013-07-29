package scala.js.exp.dom

import scala.js.exp.{FFIExp, OptionOpsExp}
import scala.js.language.dom.ElementOps
import scala.virtualization.lms.common.{EffectExp, IfThenElseExp, TupledFunctionsExp}

trait ElementOpsExp extends ElementOps with EffectExp with EventOpsExp with SelectorOpsExp with CoreExp with TupledFunctionsExp with OptionOpsExp with IfThenElseExp with FFIExp {

  def css_fontweight(css: Rep[CSSStyleDeclaration]) = foreign"$css.fontWeight"[Int].withEffect()
  def css_set_fontweight(css: Rep[CSSStyleDeclaration], v: Rep[Int]) = foreign"$css.fontWeight = $v".withEffect()

  def element_setAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) = foreign"$e.setAttribute($name, $value)".withEffect()
  def element_tagName(e: Exp[Element]) = foreign"$e.tagName"[String]
  def element_className(e: Exp[Element]) = foreign"$e.className"[String].withEffect()
  def element_parentNode(e: Exp[Element]) = foreign"$e.parentNode"[Option[Element]].withEffect()
  def element_previousSibling(e: Exp[Element]) = foreign"$e.previousSibling"[Option[Element]].withEffect()
  def element_classList(e: Exp[Element]) = foreign"$e.classList"[DOMTokenList].withEffect()
  def element_removeChild(e: Exp[Element], c: Exp[Element]) = foreign"$e.removeChild($c)".withEffect()
  def element_appendChild(e: Exp[Element], c: Exp[Element]) = foreign"$e.appendChild($c)".withEffect()
  def element_focus(e: Exp[Element]) = foreign"$e.focus()".withEffect()
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
  def element_style(e: Exp[Element]) = foreign"$e.style"[CSSStyleDeclaration].withEffect()
  
  def input_set_disabled(input: Exp[Input], value: Exp[Boolean]) = foreign"$input.disabled = $value".withEffect()
  def input_disabled(input: Exp[Input]) = foreign"$input.disabled".withEffect() // TODO Optimize effect description
  def input_set_name(input: Exp[Input], value: Exp[String]) = foreign"$input.name = $value".withEffect()
  def input_name(input: Exp[Input]) = foreign"$input.name"[String].withEffect()
  def input_value(input: Exp[Input]) = foreign"$input.value"[String].withEffect()
  def input_set_checked(input: Exp[Input], value: Exp[Boolean]) = foreign"$input.checked = $value".withEffect()
  def input_checked(input: Exp[Input]) = foreign"$input.checked"[Boolean].withEffect()

  def form_submit(form: Exp[Form]) = foreign"$form.submit()".withEffect()

}