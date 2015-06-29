package scala.js.language

import scala.language.implicitConversions

import scala.virtualization.lms.common.Base

trait Casts extends Base {
  trait AsRep {
    def as[T: Manifest]: Rep[T]
  }
  implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T: Manifest]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}
