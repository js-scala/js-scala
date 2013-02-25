package scala.js

import virtualization.lms.common._

trait JSDom { this: Base =>

  trait EventTarget
  implicit class EventTargetOps(t: Rep[EventTarget]) {
    def on(event: EventDef, capture: Rep[Boolean] = unit(false))(handler: Rep[event.Type] => Rep[Unit])(implicit m: Manifest[event.Type]) = eventtarget_on(t, new EventName[event.Type](event.name), capture, handler)
  }
  //def eventtarget_on(t: Rep[EventTarget], event: EventDef, capture: Rep[Boolean])(handler: Rep[event.Type] => Rep[Unit])(implicit m: Manifest[event.Type]): Rep[Unit]
  def eventtarget_on[A : Manifest](t: Rep[EventTarget], e: EventName[A], capture: Rep[Boolean], handler: Rep[A] => Rep[Unit]): Rep[Unit]

  trait Event
  def infix_target(e: Rep[Event]): Rep[EventTarget]

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

  trait SelectorApi
  implicit class SelectorApiOps(s: Rep[SelectorApi]) {
    def find(selector: Rep[String]) = selector_find(s, selector)
    def findAll(selector: Rep[String]) = selector_findAll(s, selector)
  }
  def selector_find(s: Rep[SelectorApi], selector: Rep[String]): Rep[Option[Element]]
  def selector_findAll(s: Rep[SelectorApi], selector: Rep[String]): Rep[List[Element]]

  trait Document extends SelectorApi with EventTarget

  trait Element extends EventTarget with SelectorApi
  implicit class ElementOps(e: Rep[Element]) {
    def setAttribute(name: Rep[String], value: Rep[_]) = element_setAttribute(e, name, value)
    def tagName = element_tagName(e)
    def parentNode = element_parentNode(e)
    def classList = element_classList(e)
    def closest(p: Rep[Element] => Rep[Boolean]) = element_closest(e, p)
    def remove() = element_remove(e)
    def removeChild(c: Rep[Element]) = element_removeChild(e, c)
  }
  def element_setAttribute(e: Rep[Element], name: Rep[String], value: Rep[_]): Rep[Unit]
  def element_tagName(e: Rep[Element]): Rep[String]
  def element_parentNode(e: Rep[Element]): Rep[Option[Element]]
  def element_classList(e: Rep[Element]): Rep[DOMTokenList]
  def element_closest(e: Rep[Element], p: Rep[Element] => Rep[Boolean]): Rep[Option[Element]]
  def element_remove(e: Rep[Element]): Rep[Unit]
  def element_removeChild(e: Rep[Element], c: Rep[Element]): Rep[Unit]

  trait DOMTokenList
  implicit class DOMTokenListOps(ts: Rep[DOMTokenList]) {
    def contains(token: Rep[String]) = domtokenlist_contains(ts, token)
    def add(tokens: Rep[String]*) = domtokenlist_add(ts, tokens)
    def remove(tokens: Rep[String]*) = domtokenlist_remove(ts, tokens)
  }
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]): Rep[Boolean]
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]): Rep[Unit]

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
    def disabled(b: Rep[Boolean]) = input_set_disabled(input, b)
  }
  def input_set_disabled(input: Rep[Input], value: Rep[Boolean]): Rep[Unit]
}

trait JSDomExp extends JSDom with EffectExp { this: JSFunctionsExp with OptionOps with IfThenElse =>
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
  def infix_target(e: Exp[Event]) = EventGetTarget(e)
  def infix_state[A : Manifest](e: Exp[PopStateEvent[A]]) = PopStateEventState(e)
  def infix_offsetX(e: Exp[MouseEvent]) = MouseEventOffsetX(e)
  def infix_offsetY(e: Exp[MouseEvent]) = MouseEventOffsetY(e)
  def infix_wheelDeltaY(e: Exp[MouseWheelEvent]) = MouseWheelEventDeltaY(e)

  def infix_document(w: Exp[Window]) = WindowDocument
  def infix_history(w: Exp[Window]) = WindowHistory

  // TODO generate a getElementById if possible
  def selector_find(s: Exp[SelectorApi], selector: Exp[String]) = reflectEffect(SelectorFind(s, selector))
  // TODO generate a getElementsByClassName or getElementsByTagName if possible
  def selector_findAll(s: Exp[SelectorApi], selector: Exp[String]) = reflectEffect(SelectorFindAll(s, selector))

  def element_setAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) = reflectEffect(ElementSetAttribute(e, name, value))
  def element_tagName(e: Exp[Element]) = ElementTagName(e)
  def element_parentNode(e: Exp[Element]) = ElementParentNode(e)
  def element_classList(e: Exp[Element]) = ElementClassList(e)
  def element_removeChild(e: Exp[Element], c: Exp[Element]) = reflectEffect(ElementRemoveChild(e, c))
  def element_closest(e: Exp[Element], p: Exp[Element] => Exp[Boolean]) = fun { e: Exp[Element] =>
    e.parentNode.fold(none, parent => if (p(parent)) some(parent) else parent.closest(p))
  } apply e
  def element_remove(e: Exp[Element]) = fun { e: Exp[Element] =>
    e.parentNode.fold((), _.removeChild(e))
  } apply e

  // TODO Check that tokens are well formed
  // TODO Handle effects in contains
  def domtokenlist_contains(ts: Rep[DOMTokenList], token: Rep[String]) = DOMTokenListContains(ts, token)
  def domtokenlist_add(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListAdd(ts, tokens))
  def domtokenlist_remove(ts: Rep[DOMTokenList], tokens: Seq[Rep[String]]) = reflectEffect(DOMTokenListRemove(ts, tokens))

  def history_replaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) = reflectEffect(HistoryReplaceState(h, state, title, url))

  def form_submit(form: Exp[Form]) = reflectEffect(FormSubmit(form))

  def input_set_disabled(input: Exp[Input], value: Exp[Boolean]) = reflectEffect(InputSetDisabled(input, value))

  case object window extends Exp[Window]

  // FIXME We can’t yet use dependent types on constructors parameters, see https://issues.scala-lang.org/browse/SI-5712 so at this point we lost the information that event.Type =:= A
  case class EventTargetOn[A](t: Exp[EventTarget], event: EventDef, capture: Exp[Boolean], e: Sym[A], handler: Block[Unit]) extends Def[Unit]
  case class EventGetTarget(e: Exp[Event]) extends Def[EventTarget]
  case class PopStateEventState[A : Manifest](e: Exp[PopStateEvent[A]]) extends Def[Option[A]]
  case class MouseEventOffsetX(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseEventOffsetY(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseWheelEventDeltaY(e: Exp[MouseWheelEvent]) extends Def[Double]

  case object WindowDocument extends Exp[Document]
  case object WindowHistory extends Exp[History]

  case class SelectorFind(s: Exp[SelectorApi], selector: Exp[String]) extends Def[Option[Element]]
  case class SelectorFindAll(s: Exp[SelectorApi], selector: Exp[String]) extends Def[List[Element]]

  case class ElementSetAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) extends Def[Unit]
  case class ElementTagName(e: Exp[Element]) extends Def[String]
  case class ElementParentNode(e: Exp[Element]) extends Def[Option[Element]]
  case class ElementClassList(e: Exp[Element]) extends Def[DOMTokenList]
  case class ElementRemoveChild(e: Exp[Element], c: Exp[Element]) extends Def[Unit]

  case class DOMTokenListContains(ts: Exp[DOMTokenList], token: Exp[String]) extends Def[Boolean]
  case class DOMTokenListAdd(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]
  case class DOMTokenListRemove(ts: Exp[DOMTokenList], tokens: Seq[Exp[String]]) extends Def[Unit]

  case class HistoryReplaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) extends Def[Unit]

  case class FormSubmit(form: Exp[Form]) extends Def[Unit]
  case class InputSetDisabled(input: Exp[Input], value: Exp[Boolean]) extends Def[Unit]

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
  val IR: EffectExp with TupledFunctionsRecursiveExp with OptionOpsExp with IfThenElseExp with JSDomExp
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
    case ElementParentNode(e) =>
      emitValDef(sym, quote(e) + ".parentNode")
    case ElementClassList(e) =>
      emitValDef(sym, quote(e) + ".classList")
    case ElementRemoveChild(e, c) =>
      emitValDef(sym, quote(e) + ".removeChild(" + quote(c) + ")")
    case DOMTokenListContains(ts, token) =>
      emitValDef(sym, quote(ts) + ".contains(" + quote(token) + ")")
    case DOMTokenListAdd(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".add(" + tokens.map(quote).mkString(", ") + ")")
    case DOMTokenListRemove(ts, tokens) =>
      emitValDef(sym, quote(ts) + ".remove(" + tokens.map(quote).mkString(", ") + ")")
    case HistoryReplaceState(h, state, title, url) =>
      emitValDef(sym, quote(h) + ".replaceState(" + quote(state) + ", " + quote(title) + ", " + quote(url) + ")")
    case FormSubmit(form) =>
      emitValDef(sym, quote(form) + ".submit()")
    case InputSetDisabled(input, value) =>
      emitAssignment(quote(input) + ".disabled", quote(value))
    case _ => super.emitNode(sym, rhs)
  }
}