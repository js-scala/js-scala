package scala.js.exp

import scala.virtualization.lms.common.BaseExp
import scala.js.language.Records

trait RecordsExp extends BaseExp with Records {
  
  def record_construct[A : Manifest](fields: (String, Exp[_])*) = {
    RecordApply[A](fields)
  }
  
  def record_select[A : Manifest, B : Manifest](obj: Exp[A], label: String) = {
    RecordSelect[A, B](obj, label)
  }
  
  case class RecordApply[A](fields: Seq[(String, Exp[_])]) extends Def[A]
  case class RecordSelect[A, B](obj: Exp[A], label: String) extends Def[B]
  
}