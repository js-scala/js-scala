package scala.js.language.dom

import scala.virtualization.lms.common.Base

trait SelectorOps extends Base with NodeListOps { this: ElementOps =>

  class Selectable[A]
  object Selectable {
    implicit val el: Selectable[Element] = new Selectable[Element]
    implicit def elSub[A <: Element]: Selectable[A] = new Selectable[A]
  }

  trait Selector
  implicit class SelectorOps(s: Rep[Selector]) {
    def find[A : Selectable : Manifest](selector: Rep[String]) = selector_find[A](s, selector)
    def findAll[A : Selectable : Manifest](selector: Rep[String]) = selector_findAll(s, selector)
  }
  // Note that selector_find[A](…)(implicit ev: A <:< Element) was simpler but sometimes didn’t make the type inferencer happy
  def selector_find[A : Selectable : Manifest](s: Rep[Selector], selector: Rep[String]): Rep[Option[A]]
  def selector_findAll[A : Selectable : Manifest](s: Rep[Selector], selector: Rep[String]): Rep[NodeList[A]]

}