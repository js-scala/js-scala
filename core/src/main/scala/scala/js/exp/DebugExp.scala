package scala.js.exp

import scala.js.language.Debug
import scala.virtualization.lms.common.EffectExp

trait DebugExp extends Debug with EffectExp {

  case class Log[A](s: Rep[A]) extends Def[Unit]

  def log[A](s: Rep[A]): Rep[Unit] = reflectEffect(Log(s))

}