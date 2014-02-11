package scala.js.language

import scala.virtualization.lms.common.Base

/**
 * Add more operations on lists than those provided by LMS.
 */
trait ListOps2 extends Base {
  implicit class ListOps2[A](l: Rep[List[A]]) {
    def foreach(f: Rep[A] => Rep[Unit])(implicit ev: Manifest[A]) = list_foreach(l, f)
    def foreachWithIndex(f: (Rep[A], Rep[Int]) => Rep[Unit])(implicit ev: Manifest[A]) = list_foreachWithIndex(l, f)
    def size = list_size(l)
  }
  def list_foreach[A : Manifest](l: Rep[List[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
  def list_foreachWithIndex[A : Manifest](l: Rep[List[A]], f: (Rep[A], Rep[Int]) => Rep[Unit]): Rep[Unit]
  def list_size[A](l: Rep[List[A]]): Rep[Int]
}