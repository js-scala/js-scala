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

  trait Window extends EventTarget

  val window: Rep[Window]
  def infix_document(w: Rep[Window]): Rep[Document]
  def infix_history(w: Rep[Window]): Rep[History]

  // Convenient aliases
  val document = window.document
  val history = window.history

  trait Document
  def infix_find(d: Rep[Document], selector: Rep[String]): Rep[Option[Element]]

  trait Element extends EventTarget
  def infix_setAttribute(e: Rep[Element], name: Rep[String], value: Rep[Any]): Rep[Unit]
  def infix_tagName(e: Rep[Element]): Rep[String]

  trait History
  implicit class HistoryOps(h: Rep[History]) {
    def replaceState(state: Rep[_], title: Rep[String], url: Rep[String]) = history_replaceState(h, state, title, url)
  }
  def history_replaceState(h: Rep[History], state: Rep[_], title: Rep[String], url: Rep[String]): Rep[Unit]

}

trait JSDomExp extends JSDom with EffectExp {
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
  def infix_find(d: Exp[Document], selector: Exp[String]) = DocumentFind(d, selector)
  def infix_setAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) = reflectEffect(ElementSetAttribute(e, name, value))
  def infix_tagName(e: Exp[Element]) = ElementTagName(e)
  def history_replaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) = reflectEffect(HistoryReplaceState(h, state, title, url))
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
  case class DocumentFind(d: Exp[Document], selector: Exp[String]) extends Def[Option[Element]]
  case class ElementSetAttribute(e: Exp[Element], name: Exp[String], value: Exp[Any]) extends Def[Unit]
  case class ElementTagName(e: Exp[Element]) extends Def[String]
  case class HistoryReplaceState(h: Exp[History], state: Exp[_], title: Exp[String], url: Exp[String]) extends Def[Unit]

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

trait JSGenDom extends JSGenEffect {
  val IR: EffectExp with JSDomExp
  import IR._

  override def quote(x: Exp[Any]) = x match {
    case `window` => "window"
    case WindowDocument => "document"
    case WindowHistory => "history"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case EventTargetOn(t, event, capture, e, handler) =>
      stream.println("var " + quote(sym) + " = " + quote(t) + ".addEventListener('" + event.name + "', function (" + quote(e) + ") {")
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
    case DocumentFind(d, selector) =>
      emitValDef(sym, quote(d) + ".querySelector(" + quote(selector) + ")")
    case ElementSetAttribute(e, name, value) =>
      emitValDef(sym, quote(e) + ".setAttribute(" + quote(name) + ", " + quote(value) + ")")
    case ElementTagName(e) =>
      emitValDef(sym, quote(e) + ".tagName")
    case HistoryReplaceState(h, state, title, url) =>
      emitValDef(sym, quote(h) + ".replaceState(" + quote(state) + ", " + quote(title) + ", " + quote(url) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}