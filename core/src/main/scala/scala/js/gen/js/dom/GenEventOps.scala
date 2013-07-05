package scala.js.gen.js.dom

import scala.js.exp.dom.EventOpsExp
import scala.js.gen.js.GenEffect

trait GenEventOps extends GenEffect {
  val IR: EventOpsExp
  import IR._

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
    case _ => super.emitNode(sym, rhs)
  }
}