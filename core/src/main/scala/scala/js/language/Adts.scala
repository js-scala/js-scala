package scala.js.language

import scala.language.experimental.macros
import scala.virtualization.lms.common.Functions

/**
 * Reifies case classes as staged adts
 * 
 * Example
 * 
 * {{{
 *   case class Point(x: Int, y: Int) extends Adt
 *   // Smart constructor
 *   val Point = adt[Point]
 *   // Members
 *   implicit def pointOps(p: Rep[Point]) = adtOps(p)
 *   
 *   // Usage
 *   def add(p1: Rep[Point], p2: Rep[Point]) =
 *     Point(p1.x + p2.x, p1.y + p2.y)
 * }}}
 */
trait Adts extends Functions {

  type Adt = scala.js.macroimpl.Adts.Adt

  def adt_construct[A : Manifest](fields: (String, Rep[_])*): Rep[A]
  def adt_select[A : Manifest, B : Manifest](obj: Rep[A], label: String): Rep[B]
  def adt_equal[A : Manifest](obj: Rep[A], bis: Rep[A], fieldsObj: Seq[String], fieldsBis: Seq[String]): Rep[Boolean]
  def adt_fold[R <: Adt : Manifest, A : Manifest](obj: Rep[R], fs: Seq[Rep[_ <: R => A]]): Rep[A]

  /**
   * {{{
   *   case class Point(x: Int, y: Int) extends Adt
   *   val Point = adt[Point]
   *   // Point is a staged smart constructor taking two Rep[Int] and returning a Rep[Point]
   * }}}
   * 
   * @return a staged smart constructor for the data type T
   */
  def adt[T <: Adt] = macro scala.js.macroimpl.Adts.adt[T]
  
  /**
   * {{{
   *   def show(point: Rep[Point]) = {
   *     implicit def pointOps(p: Rep[Point]) = adtOps(p)
   *     // Now you can select members of a Rep[Point]:
   *     "Point(x = " + point.x + ", y = " + point.y + ")"
   *   }
   *   // You also have a `copy` and an `===` method
   *   def copyAndEqual(point: Rep[Point]) = {
   *     point.copy(y = 0) === Point(42, 0)
   *   }
   * }}}
   *
   * @return an object with staged members for the type T
   */
  def adtOps[T <: Adt](o: Rep[T]) = macro scala.js.macroimpl.Adts.ops[T, Rep]

}