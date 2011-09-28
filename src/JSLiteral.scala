import scala.virtualization.lms.common._

import java.io.PrintWriter
  
trait JSLiteral extends Base with EmbeddedControls {
  type JSLiteral <: Row[Rep]
  override def __new[T, R[x]](args: (String, T => (R[t] forSome{type t}))*): T =
    newJSLiteral(args.map(_.asInstanceOf[(String, Rep[JSLiteral] => (Rep[t] forSome{type t}))]): _*).asInstanceOf[T]
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): Rep[JSLiteral]
  
  abstract class JSLiteralOps {
    def applyDynamic[T](method: String)(args: Rep[Any]*): Rep[T]
    def selectDynamic[T](field: String): Rep[T]
  }
  implicit def jsLiteralOps(receiver: Rep[JSLiteral]): JSLiteralOps
}

trait JSLiteralExp extends JSLiteral with BaseExp {
  trait JSLiteral extends Row[Rep]
  case class JSLiteralDef(args: List[(String, Exp[Any])]) extends Def[JSLiteral]
  case class DynamicCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicSelect(receiver: Exp[Any], field: String) extends Def[Any]
  class JSLiteralOpsImpl(val receiver: Exp[JSLiteral]) extends JSLiteralOps {
    def applyDynamic[T](method: String)(args: Exp[Any]*): Exp[T] =
      (DynamicCall(receiver, method, args.toList): Exp[Any]).asInstanceOf[Exp[T]]

    def selectDynamic[T](field: String): Exp[T] =
      (DynamicSelect(receiver, field): Exp[Any]).asInstanceOf[Exp[T]]
  }
  implicit def jsLiteralOps(receiver: Exp[JSLiteral]): JSLiteralOps = new JSLiteralOpsImpl(receiver)
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): Exp[JSLiteral] = {
    val evalArgs = args.toList map { case (name, f) => (name, f(null).asInstanceOf[Exp[Any]]) }
    JSLiteralDef(evalArgs)
  }
}

trait JSGenLiteral extends JSGenBase {
  val IR: JSLiteralExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case JSLiteralDef(args) => emitValDef(sym,
      args.map({case (name, value) => "'" + name + "' : " + quote(value)}).mkString("{", ",", "}"))
    case DynamicCall(receiver, method, args) =>  emitValDef(sym,
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case DynamicSelect(receiver, field) =>
      emitValDef(sym, quote(receiver) + "." + field)
    case _ => super.emitNode(sym, rhs)
  }
  }
