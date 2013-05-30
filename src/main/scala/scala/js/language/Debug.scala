package scala.js.language

import scala.virtualization.lms.common.Base

trait Debug extends Base {

  def log[A](s: Rep[A]): Rep[Unit]

}