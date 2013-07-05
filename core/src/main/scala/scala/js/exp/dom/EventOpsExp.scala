package scala.js.exp.dom

import scala.js.language.dom.EventOps
import scala.virtualization.lms.common.EffectExp

trait EventOpsExp extends EventOps with EffectExp {

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
  
  // FIXME We can’t yet use dependent types on constructors parameters, see https://issues.scala-lang.org/browse/SI-5712 so at this point we lost the information that event.Type =:= A
  case class EventTargetOn[A](t: Exp[EventTarget], event: EventDef, capture: Exp[Boolean], e: Sym[A], handler: Block[Unit]) extends Def[Unit]
  case class EventGetTarget[A](e: Exp[Event])(implicit ev: A <:< EventTarget) extends Def[A]
  case class PopStateEventState[A : Manifest](e: Exp[PopStateEvent[A]]) extends Def[Option[A]]
  case class MouseEventOffsetX(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseEventOffsetY(e: Exp[MouseEvent]) extends Def[Double]
  case class MouseWheelEventDeltaY(e: Exp[MouseWheelEvent]) extends Def[Double]
  
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