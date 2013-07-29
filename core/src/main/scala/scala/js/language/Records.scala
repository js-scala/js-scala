package scala.js.language

import scala.language.experimental.macros

import scala.virtualization.lms.common.Base

/**
 * Reifies case classes as staged records
 * 
 * Example
 * 
 * {{{
 *   case class Point(x: Int, y: Int)
 *   // Smart constructor
 *   val Point = record[Point]
 *   // Members
 *   implicit def pointOps(p: Rep[Point]) = recordOps(p)
 *   
 *   // Usage
 *   def add(p1: Rep[Point], p2: Rep[Point]) =
 *     Point(p1.x + p2.x, p1.y + p2.y)
 * }}}
 */
trait Records extends Base with Equals {

  type Record = scala.js.macroimpl.Records.Record

  def record_construct[A : Manifest](fields: (String, Rep[_])*): Rep[A]
  def record_select[A : Manifest, B : Manifest](obj: Rep[A], label: String): Rep[B]
  def record_equal[A: Manifest, Boolean : Manifest](obj: Rep[A], bis: Rep[A], fieldsObj: Seq[String], fieldsBis: Seq[String]): Rep[Boolean]

  /**
   * {{{
   *   case class Point(x: Int, y: Int)
   *   val Point = record[Point]
   *   // Point is a function taking two Rep[Int] and returning a Rep[Point]
   * }}}
   * 
   * @return a staged smart constructor for the data type T
   */
  def record[T] = macro scala.js.macroimpl.Records.record[T]
  
  /**
   * {{{
   *   def show(point: Rep[Point]) = {
   *     implicit def pointOps(p: Rep[Point]) = recordOps(p)
   *     // Now you can select members of a Rep[Point]:
   *     "Point(x = " + point.x + ", y = " + point.y + ")"
   *   }
   * }}}
   * 
   * @return an object with staged members for the type T
   */
  def recordOps[T](o: Rep[T]) = macro scala.js.macroimpl.Records.ops[T]

}