package scala.js.language

import scala.virtualization.lms.common.Base

/**
 * Language unit for optional values manipulation (similar to [[scala.Option]] type).
 */
trait OptionOps extends Base {

  val none: Rep[None.type]
  def some[A : Manifest](a: Rep[A]): Rep[Option[A]]

  implicit def OptionOpsCls[A : Manifest](o: Rep[Option[A]]): OptionOpsCls[A]
  type OptionOpsCls[+A] <: OptionOpsBase[A]

  trait OptionOpsBase[+A] {
    def foreach(f: Rep[A] => Rep[Unit]): Rep[Unit]
    def map[B : Manifest](f: Rep[A] => Rep[B]): Rep[Option[B]]
    def flatMap[B : Manifest](f: Rep[A] => Rep[Option[B]]): Rep[Option[B]]
    def isEmpty: Rep[Boolean]
    def fold[B : Manifest](none: => Rep[B], some: Rep[A] => Rep[B]): Rep[B]
    def getOrElse[B >: A : Manifest](default: => Rep[B]): Rep[B]
  }
}