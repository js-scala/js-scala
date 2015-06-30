package scala.js.language

import scala.language.implicitConversions

import scala.virtualization.lms.common.Base

trait JSLiteral extends Base with EmbeddedControls {
  trait JSLiteral extends Struct
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] =
    newJSLiteral(args.map(x => (x._1, x._3).asInstanceOf[(String, Rep[JSLiteral] => Rep[_])]): _*).asInstanceOf[Rep[T]]
  def newJSLiteral(args: (String, Rep[JSLiteral] => Rep[_])*): Rep[JSLiteral]
  
  abstract class JSLiteralOps {
    def selectDynamic[T](field: String): Rep[T]
    def applyDynamic[T](field: String) = selectDynamic[T](field)
  }
  implicit def jsLiteralOps(receiver: Rep[JSLiteral]): JSLiteralOps
}
