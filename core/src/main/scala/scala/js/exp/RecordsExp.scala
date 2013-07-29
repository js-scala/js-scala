package scala.js.exp

import scala.virtualization.lms.common.{BaseExp, EqualExp}
import scala.js.language.Records

trait RecordsExp extends BaseExp with Records with EqualExp {
  
  def record_construct[A : Manifest](fields: (String, Exp[_])*) = {
    RecordApply[A](fields)
  }
  
  def record_select[A : Manifest, B : Manifest](obj: Exp[A], label: String) = {
    RecordSelect[A, B](obj, label)
  }
  
  def record_equal[A : Manifest, Boolean : Manifest](obj: Exp[A], bis: Exp[A], fieldsObj: Seq[String], fieldsBis: Seq[String]) = {
    RecordEqual[A, Boolean](obj, bis, fieldsObj, fieldsBis)
  }
  
  case class RecordApply[A](fields: Seq[(String, Exp[_])]) extends Def[A]
  case class RecordSelect[A, B](obj: Exp[A], label: String) extends Def[B]
  case class RecordEqual[A, Boolean](obj: Exp[A], bis: Exp[A], fieldsObj: Seq[String], fieldsBis: Seq[String]) extends Def[Boolean]
  
}