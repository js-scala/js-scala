package scala.js

import virtualization.lms.common._

trait JSDom extends Base {

  trait EventTarget
  implicit class EventTargetOps(t: Rep[EventTarget]) {
    def on(event: EventDef, capture: Rep[Boolean] = unit(false))(handler: Rep[event.Type] => Rep[Unit])(implicit m: Manifest[event.Type]) = eventtarget_on(t, new EventName[event.Type](event.name), capture, handler)
  }
  //def eventtarget_on(t: Rep[EventTarget], event: EventDef, capture: Rep[Boolean])(handler: Rep[event.Type] => Rep[Unit])(implicit m: Manifest[event.Type]): Rep[Unit]
  def eventtarget_on[A : Manifest](t: Rep[EventTarget], e: EventName[A], capture: Rep[Boolean], handler: Rep[A] => Rep[Unit]): Rep[Unit]

  trait Event
  implicit class EventOps(e: Rep[Event]) {
    def target[A : Manifest](implicit ev: A <:< EventTarget) = event_target[A](e)
  }
  def event_target[A : Manifest](e: Rep[Event])(implicit ev: A <:< EventTarget): Rep[A]

  class EventDef(val name: String) {
    type Type
  }

  class EventName[A](name: String) extends EventDef(name) {
    type Type = A
  }

  trait PopStateEvent[A] extends Event
  def infix_state[A : Manifest](e: Rep[PopStateEvent[A]]): Rep[Option[A]]

  class PopState[A] extends EventName[PopStateEvent[A]]("popstate")
  object PopState {
    def apply[A] = new PopState[A]
  }

  trait MouseEvent extends Event
  def infix_offsetX(e: Rep[MouseEvent]): Rep[Double]
  def infix_offsetY(e: Rep[MouseEvent]): Rep[Double]

  trait MouseWheelEvent extends MouseEvent
  def infix_wheelDeltaY(e: Rep[MouseWheelEvent]): Rep[Double]

  case object MouseWheel extends EventName[MouseWheelEvent]("mousewheel")
  case object MouseDown extends EventName[MouseEvent]("mousedown")
  case object MouseMove extends EventName[MouseEvent]("mousemove")
  case object MouseUp extends EventName[MouseEvent]("mouseup")
  case object Click extends EventName[MouseEvent]("click")

  case object Change extends EventName[Event]("change")
  case object Submit extends EventName[Event]("submit")

  trait Window extends EventTarget

  val window: Rep[Window]
  def infix_document(w: Rep[Window]): Rep[Document]
  def infix_history(w: Rep[Window]): Rep[History]

  // Convenient aliases
  val document = window.document
  val history = window.history

  class Selectable[A]
  object Selectable {
    implicit val el: Selectable[Element] = new Selectable[Element]
    implicit def elSub[A <: Element]: Selectable[A] = new Selectable[A]
  }

  trait SelectorApi
  implicit class SelectorApiOps(s: Rep[SelectorApi]) {
    def find[A : Selectable : Manifest](selector: Rep[String]) = selector_find[A](s, selector)
    def findAll[A : Selectable : Manifest](selector: Rep[String]) = selector_findAll(s, selector)
  }
  // Note that selector_find[A](…)(implicit ev: A <:< Element) was simpler but sometimes didn’t make the type inferencer happy
  def selector_find[A : Selectable : Manifest](s: Rep[SelectorApi], selector: Rep[String]): Rep[Option[A]]
  def selector_findAll[A : Selectable : Manifest](s: Rep[SelectorApi], selector: Rep[String]): Rep[List[A]]

  trait Document extends SelectorApi with EventTarget

  trait Element extends EventTarget with SelectorApi
  implicit class ElementOps(e: Rep[Element]) {
    def setAttribute(name: Rep[String], value: Rep[_]) = element_setAttribute(e, name, value)
    def tagName = element_tagName(e)
    def className = element_className(e)
    def parentNode = element_parentNode(e)
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
  def element_previousSibling(e: Rep[Element]): Rep[Option[Element]]
  def element_classList(e: Rep[Element]): Rep[DOMTokenList]
  def element_closest(e: Rep[Element], p: Rep[Element] => Rep[Boolean]): Rep[Option[Element]]
  def element_prev(e: Rep[Element], p: Rep[Element] => Rep[Boolean]): Rep[Option[Element]]
  def element_remove(e: Rep[Element]): Rep[Unit]
  def element_removeChild(e: Rep[Element], c: Rep[Element]): Rep[Unit]
  def element_appendChild(e: Rep[Element], c: Rep[Element]): Rep[Unit]
  def element_focus(e: Rep[Element]): Rep[Unit]
  def element_style(e: Rep[Element]): Rep[CSSStyleDeclaration]

  trait DOMTokenList
  implicit class DOMTokenListOps(ts: Rep[DOMTokenList]) {
    def contains(token: Rep[String]) = domtokenlist_contains(ts, token)
    def add(tokens: Rep[String]*) = domtokenlist_add(ts, tokens)
    def remove(tokens: Rep[String]*) = domtokenlist_remove(ts, tokens)
  }
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]): Rep[Boolean]
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]

  trait CSSStyleDeclaration
  implicit class CSSStyleDeclarationOps(css: Rep[CSSStyleDeclaration]) {
    def fontWeight = css_fontweight(css)
    def fontWeight_=(v: Rep[Int]) = css_set_fontweight(css, v)
  }
  def css_fontweight(css: Rep[CSSStyleDeclaration]): Rep[Int]
  def css_set_fontweight(css: Rep[CSSStyleDeclaration], v: Rep[Int]): Rep[Unit]

  trait History
  implicit class HistoryOps(h: Rep[History]) {
    def replaceState(state: Rep[_], title: Rep[String], url: Rep[String]) = history_replaceState(h, state, title, url)
  }
  def history_replaceState(h: Rep[History], state: Rep[_], title: Rep[String], url: Rep[String]): Rep[Unit]

  trait Form extends Element
  implicit class FormOps(form: Rep[Form]) {
    def submit() = form_submit(form)
  }
  def form_submit(form: Rep[Form]): Rep[Unit]

  trait Input extends Element
  implicit class InputOps(input: Rep[Input]) {
    def disabled_=(b: Rep[Boolean]) = input_set_disabled(input, b)
    def disabled = input_disabled(input)
    def name_=(n: Rep[String]) = input_set_name(input, n)
    def name = input_name(input)
    def value = input_value(input)
  }
  def input_set_disabled(input: Rep[Input], value: Rep[Boolean]): Rep[Unit]
  def input_disabled(input: Rep[Input]): Rep[Boolean]
  def input_set_name(input: Rep[Input], value: Rep[String]): Rep[Unit]
  def input_name(input: Rep[Input]): Rep[String]
  def input_value(input: Rep[Input]): Rep[String]
}

trait JSDomExp extends JSDom with EffectExp with JSFunctionsExp with OptionOpsExp with IfThenElseExp {

  /*def eventtarget_on(t: Exp[EventTarget], event: EventDef, capture: Exp[Boolean])(handler: Exp[event.Type] => Exp[Unit])(implicit m: Manifest[event.Type]) = {
    val e = fresh[event.Type]
    val block = reifyEffects(handler(e))
    reflectEffect(EventTargetOn(t, event, capture, e, block))
  }*/
  def eventtarget_on[A : Manifest](t: Exp[EventTarget], event: EventName[A], capture: Exp[Boolean], handler: Exp[A] => Exp[Unit]) = {
    val e = fresh[A]
    val b = reifyEffects(handler(e))
    reflectEffect(EventTargetOn(t, event, capture, e, b))
  }
  def event_target[A : Manifest](e: Exp[Event])(implicit ev: A <:< EventTarget) = EventGetTarget[A](e)
  def infix_state[A : Manifest](e: Exp[PopStateEvent[A]]) = PopStateEventState(e)
  def infix_offsetX(e: Exp[MouseEvent]) = MouseEventOffsetX(e)
  def infix_offsetY(e: Exp[MouseEvent]) = MouseEventOffsetY(e)
  def infix_wheelDeltaY(e: Exp[MouseWheelEvent]) = MouseWheelEventDeltaY(e)

  def infix_document(w: Exp[Window]) = WindowDocument
  def infix_history(w: Exp[Window]) = WindowHistory

  // TODO generate a getElementById if possible
  def selector_find[A : Selectable : Manifest](s: Exp[SelectorApi], selector: Exp[String]) =
    reflectEffect(SelectorFind[A](s, selector))
  // TODO generate a getElementsByClassName or getElementsByTagName if possible
  def selector_findAll[A : Selectable : Manifest](s: Exp[SelectorApi], selector: Exp[String]) = {
    val ns = reflectEffect(SelectorFindAll[A](s, selector))
    val toArray: Exp[NodeList => List[A]] = NodeListToArray[A]()
    toArray(ns)
  }
  trait NodeList

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
      e.parentNode.fold(none, parent => if (p(parent)) some(parent) else element_closest_reified(parent, p))
    }
  def element_closest(e: Exp[Element], p: Exp[Element] => Exp[Boolean]) = element_closest_reified(e, p)
  def element_prev_reified: Exp[((Element, Element => Boolean)) => Option[Element]] =
    fun { (e: Exp[Element], p: Exp[Element => Boolean]) =>
      e.previousSibling.fold(none, sib => if (p(sib)) some(sib) else element_prev_reified(sib, p))
    }
  def element_prev(e: Exp[Element], p: Exp[Element] => Exp[Boolean]) = element_prev_reified(e, p)
  def element_remove(e: Exp[Element]) = fun { e: Exp[Element] =>
    e.parentNode.fold((), _.removeChild(e))
  } apply e
  def element_style(e: Exp[Element]) = ElementStyle(e)

  // TODO Check that tokens are well formed
  // TODO Handle effects in contains
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]) = DOMTokenListContains(ts, token)
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListAdd(ts, tokens))
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListRemove(ts, tokens))

  def css_fontweight(css: Rep[CSSStyleDeclaration]) = reflectEffect(CSSFontWeight(css))
  def css_set_fontweight(css: Rep[CSSStyleDeclaration], v: Rep[Int]) = reflectEffect(CSSSetFontWeight(css, v))

  def history_replaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) = reflectEffect(HistoryReplaceState(h, state, title, url))

  def form_submit(form: Exp[Form]) = reflectEffect(FormSubmit(form))

  def input_set_disabled(input: Exp[Input], value: Exp[Boolean]) = reflectEffect(InputSetDisabled(input, value))
  def input_disabled(input: Exp[Input]) = reflectEffect(InputDisabled(input)) // TODO Optimize effect description
  def input_set_name(input: Exp[Input], value: Exp[String]) = reflectEffect(InputSetName(input, value))
  def input_name(input: Exp[Input]) = reflectEffect(InputName(input))
  def input_value(input: Exp[Input]) = reflectEffect(InputValue(input))

  case object window extends Exp[Window]

  // FIXME We can’t yet use dependent types on constructors parameters, see https://issues.scala-lang.org/browse/SI-5712 so at this point we lost the information that event.Type =:= A
  case class EventTargetOn[A](t: Exp[EventTarget], event: EventDef, capture: Exp[Boolean], e: Sym[A], handler: Block[Unit]) extends Def[Unit]
  case class EventGetTarget[A](e: Exp[Event])(implicit ev: A <:< EventTarget) extends Def[A]
  case class PopStateEventState[A : Manifest](e: Exp[PopStateEvent[A]]) extends Def[Option[A]]
  case class MouseEventOffsetX(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseEventOffsetY(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseWheelEventDeltaY(e: Exp[MouseWheelEvent]) extends Def[Double]

  case object WindowDocument extends Exp[Document]
  case object WindowHistory extends Exp[History]

  case class SelectorFind[A : Selectable](s: Exp[SelectorApi], selector: Exp[String]) extends Def[Option[A]]
  case class SelectorFindAll[A : Selectable](s: Exp[SelectorApi], selector: Exp[String]) extends Def[NodeList]

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

  case class DOMTokenListContains(ts: Exp[DOMTokenList], token: Exp[String]) extends Def[Boolean]
  case class DOMTokenListAdd(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]
  case class DOMTokenListRemove(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]

  case class CSSFontWeight(css: Exp[CSSStyleDeclaration]) extends Def[Int]
  case class CSSSetFontWeight(css: Exp[CSSStyleDeclaration], v: Exp[Int]) extends Def[Unit]

  case class HistoryReplaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) extends Def[Unit]

  case class FormSubmit(form: Exp[Form]) extends Def[Unit]
  case class InputSetDisabled(input: Exp[Input], value: Exp[Boolean]) extends Def[Unit]
  case class InputDisabled(input: Exp[Input]) extends Def[Boolean]
  case class InputSetName(input: Exp[Input], value: Exp[String]) extends Def[Unit]
  case class InputName(input: Exp[Input]) extends Def[String]
  case class InputValue(input: Exp[Input]) extends Def[String]

  case class NodeListToArray[A : Selectable]() extends Def[NodeList => List[A]]

  override def syms(e: Any) = e match {
    case EventTargetOn(t, event, capture, _, handler) => List(t, event, capture, handler).flatMap(syms)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case EventTargetOn(_, _, _, e, handler) => e :: effectSyms(handler)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case EventTargetOn(t, event, capture, _, handler) => List(t, event, capture, handler).flatMap(freqNormal)
    case _ => super.symsFreq(e)
  }

}

trait JSGenDom extends JSGenEffect with JSGenFunctions with JSGenOptionOps with JSGenIfThenElse {
  val IR: JSDomExp
  import IR._

  override def quote(x: Exp[Any]) = x match {
    case `window` => "window"
    case WindowDocument => "document"
    case WindowHistory => "history"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EventTargetOn(t, event, capture, e, handler) =>
      stream.println(quote(t) + ".addEventListener('" + event.name + "', function (" + quote(e) + ") {")
      emitBlock(handler)
      stream.println("}, " + quote(capture) + ");")
    case EventGetTarget(e) =>
      emitValDef(sym, quote(e) + ".target")
    case PopStateEventState(e) =>
      emitValDef(sym, quote(e) + ".state")
    case MouseEventOffsetX(e) =>
      emitValDef(sym, quote(e) + ".offsetX")
    case MouseEventOffsetY(e) =>
      emitValDef(sym, quote(e) + ".offsetY")
    case MouseWheelEventDeltaY(e) =>
      emitValDef(sym, quote(e) + ".wheelDeltaY")
    case SelectorFind(s, selector) =>
      emitValDef(sym, quote(s) + ".querySelector(" + quote(selector) + ")")
    case SelectorFindAll(s, selector) =>
      emitValDef(sym, quote(s) + ".querySelectorAll(" + quote(selector) + ")")
    case ElementSetAttribute(e, name, value) =>
      emitValDef(sym, quote(e) + ".setAttribute(" + quote(name) + ", " + quote(value) + ")")
    case ElementTagName(e) =>
      emitValDef(sym, quote(e) + ".tagName")
    case ElementClassName(e) =>
      emitValDef(sym, quote(e) + ".className")
    case ElementParentNode(e) =>
      emitValDef(sym, quote(e) + ".parentNode")
    case ElementPreviousSibling(e) =>
      emitValDef(sym, quote(e) + ".previousSibling")
    case ElementClassList(e) =>
      emitValDef(sym, quote(e) + ".classList")
    case ElementRemoveChild(e, c) =>
      emitValDef(sym, quote(e) + ".removeChild(" + quote(c) + ")")
    case ElementAppendChild(e, c) =>
      emitValDef(sym, quote(e) + ".appendChild(" + quote(c) + ")")
    case ElementFocus(e) =>
      emitValDef(sym, quote(e) + ".focus()")
    case ElementStyle(e) =>
      emitValDef(sym, quote(e) + ".style")
    case DOMTokenListContains(ts, token) =>
      emitValDef(sym, quote(ts) + ".contains(" + quote(token) + ")")
    case DOMTokenListAdd(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".add(" + tokens.map(quote).mkString(", ") + ")")
    case DOMTokenListRemove(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".remove(" + tokens.map(quote).mkString(", ") + ")")
    case CSSFontWeight(css) =>
      emitValDef(sym, quote(css) + ".fontWeight")
    case CSSSetFontWeight(css, v) =>
      emitAssignment(s"${quote(css)}.fontWeight", quote(v))
    case HistoryReplaceState(h, state, title, url) =>
      emitValDef(sym, quote(h) + ".replaceState(" + quote(state) + ", " + quote(title) + ", " + quote(url) + ")")
    case FormSubmit(form) =>
      emitValDef(sym, quote(form) + ".submit()")
    case InputSetDisabled(input, value) =>
      emitAssignment(quote(input) + ".disabled", quote(value))
    case InputDisabled(input) =>
      emitValDef(sym, quote(input) + ".disabled")
    case InputSetName(input, name) =>
      emitAssignment(s"${quote(input)}.name", quote(name))
    case InputName(input) =>
      emitValDef(sym, s"${quote(input)}.name")
    case InputValue(input) =>
      emitValDef(sym, s"${quote(input)}.value")
    case NodeListToArray() =>
      emitValDef(sym, "function (ns) { var r = []; for (var i = 0, l = ns.length ; i < l ; i++) r.push(ns.item(i)) ; return r }")
    case _ => super.emitNode(sym, rhs)
  }
}