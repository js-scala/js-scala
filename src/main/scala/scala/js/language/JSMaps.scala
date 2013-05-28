package scala.js.language

import scala.virtualization.lms.common.Base

/** Support for scala map operators in javascript.
 *
 *  @author Lucas Satabin
 */
trait JSMaps extends Base {

  object JSMap {
    def apply[K:JSKey,V:Manifest]() = map_new[K,V]()
  }

  /* only some types are authorized as keys to avoid problems in javascript
   * authorized keys are of type:
   *  - String
   *  - Char
   *  - numeric types (Int, Long, Float, Double, Byte, ...)
   *  - Boolean
   */
  abstract class JSKey[A:Manifest] extends Manifest[A] {
    def runtimeClass = implicitly[Manifest[A]].runtimeClass
  }
  object JSKey {
    implicit val stringKey = new JSKey[String] {}
    implicit val charKey = new JSKey[Char] {}
    implicit val intKey = new JSKey[Int] {}
    implicit val longKey = new JSKey[Long] {}
    implicit val floatKey = new JSKey[Float] {}
    implicit val doubleKey = new JSKey[Double] {}
    implicit val boolKey = new JSKey[Boolean] {}
  }

  implicit class mapOpsCls[K:JSKey,V:Manifest](m: Rep[Map[K,V]]) {
    def apply(k: Rep[K]) = map_apply(m, k)
    def get(k: Rep[K]) = map_get(m, k)
    def update(k: Rep[K], v: Rep[V]) = map_update(m,k,v)
    def contains(k: Rep[K]) = map_contains(m, k)
    def size = map_size(m)
    def values = map_values(m)
    def clear() = map_clear(m)
    def keys = map_keys(m)
    def foreach(block: ((Rep[K], Rep[V])) => Rep[Unit]) = map_foreach(m, block)
    def mapValues[U:Manifest](block: Rep[V] => Rep[U]) = map_mapValues(m, block)
    def filter(block: ((Rep[K], Rep[V])) => Rep[Boolean]) = map_filter(m, block)
  }

  def map_new[K:JSKey,V:Manifest]() : Rep[Map[K,V]]
  def map_apply[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K]): Rep[V]
  def map_get[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K]): Rep[Option[V]]
  def map_update[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K], v: Rep[V]): Rep[Unit]
  def map_contains[K:JSKey,V:Manifest](m: Rep[Map[K,V]], i: Rep[K]): Rep[Boolean]
  def map_size[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Int]
  def map_values[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Array[V]]
  def map_clear[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Unit]
  def map_keys[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Array[K]]
  def map_foreach[K:JSKey,V:Manifest](m: Rep[Map[K,V]], block: ((Rep[K], Rep[V])) => Rep[Unit]): Rep[Unit]
  def map_mapValues[K:JSKey,V:Manifest,U:Manifest](m: Rep[Map[K,V]], block: Rep[V] => Rep[U]): Rep[Map[K,U]]
  def map_filter[K:JSKey,V:Manifest](m: Rep[Map[K,V]], block: ((Rep[K], Rep[V])) => Rep[Boolean]): Rep[Map[K,V]]
}