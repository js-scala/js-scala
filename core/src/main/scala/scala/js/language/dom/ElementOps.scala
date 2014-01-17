package scala.js.language.dom

import scala.virtualization.lms.common.Base

/**
 * [[org.w3c.dom.Element]] manipulation.
 */
trait ElementOps extends Base with EventOps with SelectorOps with Core {

  trait CSSStyleDeclaration
  implicit class CSSStyleDeclarationOps(css: Rep[CSSStyleDeclaration]) {
    def fontWeight = css_fontweight(css)
    def fontWeight_=(v: Rep[Int]) = css_set_fontweight(css, v)
  }
  def css_fontweight(css: Rep[CSSStyleDeclaration]): Rep[Int]
  def css_set_fontweight(css: Rep[CSSStyleDeclaration], v: Rep[Int]): Rep[Unit]

  trait Element extends EventTarget with Selector
  implicit class ElementOps(e: Rep[Element]) {
    def setAttribute(name: Rep[String], value: Rep[_]) = element_setAttribute(e, name, value)
    def tagName = element_tagName(e)
    def className = element_className(e)
    def parentNode = element_parentNode(e)
    def firstChild = element_firstChild(e)
    def previousSibling = element_previousSibling(e)
    def classList = element_classList(e)
    def closest(p: Rep[Element] => Rep[Boolean]) = element_closest(e, p)
    def prev(p: Rep[Element] => Rep[Boolean]) = element_prev(e, p)
    def remove() = element_remove(e)
    def removeChild(c: Rep[Element]) = element_removeChild(e, c)
    def appendChild(c: Rep[Element]) = element_appendChild(e, c)
    def focus() = element_focus(e)
    def style = element_style(e)
  }
  def element_setAttribute(e: Rep[Element], name: Rep[String], value: Rep[_]): Rep[Unit]
  def element_tagName(e: Rep[Element]): Rep[String]
  def element_className(e: Rep[Element]): Rep[String]
  def element_parentNode(e: Rep[Element]): Rep[Option[Element]]
  def element_firstChild(e: Rep[Element]): Rep[Option[Element]]
  def element_previousSibling(e: Rep[Element]): Rep[Option[Element]]
  def element_classList(e: Rep[Element]): Rep[DOMTokenList]
  def element_closest(e: Rep[Element], p: Rep[Element] => Rep[Boolean]): Rep[Option[Element]]
  def element_prev(e: Rep[Element], p: Rep[Element] => Rep[Boolean]): Rep[Option[Element]]
  def element_remove(e: Rep[Element]): Rep[Unit]
  def element_removeChild(e: Rep[Element], c: Rep[Element]): Rep[Unit]
  def element_appendChild(e: Rep[Element], c: Rep[Element]): Rep[Unit]
  def element_focus(e: Rep[Element]): Rep[Unit]
  def element_style(e: Rep[Element]): Rep[CSSStyleDeclaration]

  trait Input extends Element
  implicit class InputOps(input: Rep[Input]) {
    def disabled_=(b: Rep[Boolean]) = input_set_disabled(input, b)
    def disabled = input_disabled(input)
    def name_=(n: Rep[String]) = input_set_name(input, n)
    def name = input_name(input)
    def value = input_value(input)
    def checked_=(b: Rep[Boolean]) = input_set_checked(input, b)
    def checked = input_checked(input)
  }
  def input_set_disabled(input: Rep[Input], value: Rep[Boolean]): Rep[Unit]
  def input_disabled(input: Rep[Input]): Rep[Boolean]
  def input_set_name(input: Rep[Input], value: Rep[String]): Rep[Unit]
  def input_name(input: Rep[Input]): Rep[String]
  def input_value(input: Rep[Input]): Rep[String]
  def input_set_checked(input: Rep[Input], value: Rep[Boolean]): Rep[Unit]
  def input_checked(input: Rep[Input]): Rep[Boolean]

  trait Form extends Element
  implicit class FormOps(form: Rep[Form]) {
    def submit() = form_submit(form)
  }
  def form_submit(f: Rep[Form]): Rep[Unit]

  trait Img extends Element
  implicit class ImgOps(img: Rep[Img]) {
    def src = img_src(img)
    def src_=(src: Rep[String]) = img_set_src(img, src)
  }
  def img_src(img: Rep[Img]): Rep[String]
  def img_set_src(img: Rep[Img], src: Rep[String]): Rep[Unit]

}