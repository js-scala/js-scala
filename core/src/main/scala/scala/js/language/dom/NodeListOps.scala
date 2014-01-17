package scala.js.language.dom

import scala.virtualization.lms.common.Base

/**
 * [[org.w3c.dom.NodeList]] manipulation.
 */
trait NodeListOps extends Base {

  class NodeList[A]
  implicit class NodeListOps[A : Manifest](ns: Rep[NodeList[A]]){
    def size = nodeList_size(ns)
    def filter(f: Rep[A] => Rep[Boolean]) = nodeList_filter(ns, f)
    def foreach(f: Rep[A] => Rep[Unit]) = nodeList_foreach(ns, f)
    def map[B : Manifest](f: Rep[A] => Rep[B]) = nodeList_map(ns, f)
    def foreachWithIndex(f: (Rep[A], Rep[Int]) => Rep[Unit]) = nodeList_foreachWithIndex(ns, f)
  }
  def nodeList_size[A : Manifest](s: Rep[NodeList[A]]): Rep[Int]
  def nodeList_filter[A : Manifest](ns: Rep[NodeList[A]], f: Rep[A] => Rep[Boolean]): Rep[List[A]]
  def nodeList_foreach[A : Manifest](ns: Rep[NodeList[A]], f: Rep[A] => Rep[Unit]) : Rep[Unit]
  def nodeList_map[A : Manifest, B : Manifest](ns: Rep[NodeList[A]], f: Rep[A] => Rep[B]): Rep[List[B]]
  def nodeList_foreachWithIndex[A : Manifest](ns: Rep[NodeList[A]], f: (Rep[A], Rep[Int]) => Rep[Unit]): Rep[Unit]

}