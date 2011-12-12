package scala.js

import scala.virtualization.lms.common._

trait Casts extends Base {
  trait AsRep {
    def as[T: Manifest]: Rep[T]
  }
  implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T: Manifest]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}
