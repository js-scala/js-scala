package scala.js.language.dom

import scala.virtualization.lms.common.Base
 
trait EventOps extends Base {

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

}