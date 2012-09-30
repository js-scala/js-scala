package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait JSDebug extends Base {

  def log[A](s: Rep[A]): Rep[Unit]

}

trait JSDebugExp extends JSDebug with EffectExp {

  case class Log[A](s: Rep[A]) extends Def[Unit]

  def log[A](s: Rep[A]): Rep[Unit] = reflectEffect(Log(s))

}

trait JSGenDebug extends JSGenEffect {
  val IR: JSDebugExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Log(s) => emitValDef(sym, "console.log(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }

}
