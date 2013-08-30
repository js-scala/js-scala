package scala.js.exp

import scala.virtualization.lms.common.{BaseExp, EqualExp, TupledFunctionsExp}
import scala.js.language.Adts

trait AdtsExp extends BaseExp with Adts with TupledFunctionsExp with EqualExp {
  
  def adt_construct[A : Manifest](fields: (String, Exp[_])*) = {
    AdtApply[A](fields)
  }
  
  def adt_select[A : Manifest, B : Manifest](obj: Exp[A], label: String) = {
    AdtSelect[A, B](obj, label)
  }
  
  def adt_equal[A : Manifest, Boolean : Manifest](obj: Exp[A], bis: Exp[A], fieldsObj: Seq[String], fieldsBis: Seq[String]) = {
    AdtEqual[A, Boolean](obj, bis, fieldsObj, fieldsBis)
  }

  def adt_fold[R : Manifest, A : Manifest](obj: Exp[R], fs: Seq[Exp[_ <: R => A]]) = {
    AdtFold[R, A](obj, fs)
  }

  case class AdtApply[A](fields: Seq[(String, Exp[_])]) extends Def[A]
  case class AdtSelect[A, B](obj: Exp[A], label: String) extends Def[B]
  case class AdtEqual[A, Boolean](obj: Exp[A], bis: Exp[A], fieldsObj: Seq[String], fieldsBis: Seq[String]) extends Def[Boolean]
  case class AdtFold[R, A](obj: Exp[R], fs: Seq[Exp[_ <: R => A]]) extends Def[A]
  
}