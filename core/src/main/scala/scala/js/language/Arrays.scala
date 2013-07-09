package scala.js.language

import scala.virtualization.lms.common.Base

trait Arrays extends Base {
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]): ArrayOps[T] = new ArrayOps(a)

  class ArrayOps[T:Manifest](a: Rep[Array[T]]) {
    def apply(i: Rep[Int]) = array_apply(a, i)
    def length = array_length(a)
    def update(i: Rep[Int], x: Rep[T]) = array_update(a, i, x)
    def foreach(block: Rep[T] => Rep[Unit]) = array_foreach(a, block)
    def map[U:Manifest](block: Rep[T] => Rep[U]) = array_map(a, block)
    def flatMap[U:Manifest](block: Rep[T] => Rep[Array[U]]) = array_flatMap(a, block)
    def filter(block: Rep[T] => Rep[Boolean]) = array_filter(a, block)
    def join(s: Rep[String]) = array_join(a, s)
    def toList: Rep[List[T]] = array_tolist(a)
  }

  def array[T:Manifest](xs: Rep[T]*): Rep[Array[T]]
  def array_apply[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def array_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def array_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  def array_foreach[T:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def array_map[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[U]): Rep[Array[U]]
  def array_flatMap[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Array[U]]): Rep[Array[U]]
  def array_filter[T:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Boolean]): Rep[Array[T]]
  def array_join[T:Manifest](a: Rep[Array[T]], s: Rep[String]): Rep[String]
  def array_tolist[T : Manifest](a: Rep[Array[T]]): Rep[List[T]]

  case class Range(a: Rep[Int], b: Rep[Int]) {
    def foreach(block: Rep[Int] => Rep[Unit]) = range_foreach(this, block)
    def map[U:Manifest](block: Rep[Int] => Rep[U]) = range_map(this, block)
    def flatMap[U:Manifest](block: Rep[Int] => Rep[Array[U]]) = range_flatMap(this, block)
    def filter(block: Rep[Int] => Rep[Boolean]) = range_filter(this, block)
  }
  def range(a: Rep[Int], b: Rep[Int]) = Range(a, b)
  def range_foreach(r: Range, block: Rep[Int] => Rep[Unit]): Rep[Unit]
  def range_map[U:Manifest](r: Range, block: Rep[Int] => Rep[U]): Rep[Array[U]]
  def range_flatMap[U:Manifest](r: Range, block: Rep[Int] => Rep[Array[U]]): Rep[Array[U]]
  def range_filter(r: Range, block: Rep[Int] => Rep[Boolean]): Rep[Array[Int]]
}