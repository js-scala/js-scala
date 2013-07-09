package scala.js.exp

import scala.js.language.Casts
import scala.virtualization.lms.common.EffectExp

trait CastsCheckedExp extends Casts with EffectExp {
  case class Cast[T](x: Rep[_], m: Manifest[T]) extends Def[T]
  override implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T: Manifest]: Rep[T] = reflectEffect(Cast[T](x, implicitly[Manifest[T]]))
  }
}
