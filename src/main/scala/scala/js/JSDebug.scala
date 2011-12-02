package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait JSDebug extends Base {

  def alert(s: Rep[String]): Rep[Unit]

}

trait JSDebugExp extends JSDebug with EffectExp {

  case class Alert(s: Rep[String]) extends Def[Unit]

  def alert(s: Rep[String]): Rep[Unit] = reflectEffect(Alert(s))

}

trait JSGenDebug extends JSGenEffect {
  val IR: JSDebugExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Alert(s) => emitValDef(sym, "console.log(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}
