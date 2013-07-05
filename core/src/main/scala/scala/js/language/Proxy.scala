package scala.js.language

import scala.virtualization.lms.common.Base

trait Proxy extends Base {
  def repProxy[T<:AnyRef](x: Rep[T])(implicit m: Manifest[T]): T
}