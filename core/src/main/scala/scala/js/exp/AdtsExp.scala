package scala.js.exp

import scala.virtualization.lms.common.{TupledFunctionsExp, BaseExp}
import scala.js.language.Adts

trait AdtsExp extends Adts with BaseExp with TupledFunctionsExp {
  
  def adt_construct[A : Manifest](fields: (String, Exp[_])*) = {
    AdtApply[A](fields)
  }
  
  def adt_select[A : Manifest, B : Manifest](obj: Exp[A], label: String) = {
    AdtSelect[A, B](obj, label)
  }
  
  def adt_equal[A : Manifest](a1: Exp[A], a2: Exp[A], fields: Seq[Exp[Boolean]]) = {
    AdtEqual[A](a1, a2, fields)
  }

  def adt_field_equal[A](a1: Exp[A], a2: Exp[A], field: String) = AdtFieldEqual(a1, a2, field)

  def adt_fold[R : Manifest, A : Manifest](obj: Exp[R], fs: Seq[Exp[_ <: R => A]]) = {
    AdtFold[R, A](obj, fs)
  }

  case class AdtApply[A](fields: Seq[(String, Exp[_])]) extends Def[A]
  case class AdtSelect[A, B](obj: Exp[A], label: String) extends Def[B]
  case class AdtEqual[A](obj: Exp[A], bis: Exp[A], fieldsObj: Seq[Exp[Boolean]]) extends Def[Boolean]
  case class AdtFieldEqual[A](a1: Exp[A], a2: Exp[A], field: String) extends Def[Boolean]
  case class AdtFold[R, A](obj: Exp[R], fs: Seq[Exp[_ <: R => A]]) extends Def[A]
  
}