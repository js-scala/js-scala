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

  case class JSLiteralDef(members: List[(String, Exp[Any])]) extends Def[JSLiteral]
  case class MemberCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class MemberSelect(receiver: Exp[Any], field: String) extends Def[Any]
  case class Member(value: Exp[Any]) extends Def[Any]
  case class JSLiteralSelf(map: scala.collection.mutable.Map[String, Exp[Any]]) extends Exp[JSLiteral]

  class JSLiteralOpsImpl(val receiver: Exp[JSLiteral]) extends JSLiteralOps {
    def applyDynamic[T](method: String)(args: Exp[Any]*): Exp[T] =
      (MemberCall(receiver, method, args.toList): Exp[Any]).asInstanceOf[Exp[T]]

    def selectDynamic[T](field: String): Exp[T] =
      (MemberSelect(receiver, field): Exp[Any]).asInstanceOf[Exp[T]]
  }
  implicit def jsLiteralOps(receiver: Exp[JSLiteral]): JSLiteralOps = new JSLiteralOpsImpl(receiver)
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): Exp[JSLiteral] = {
    def ensureSym(exp: Exp[Any]): Exp[Any] = exp match {
      case Sym(_) => exp
      case _ => Member(exp)
    }
    val map = scala.collection.mutable.Map[String, Exp[Any]]()
    val evalArgs = args.toList map { case (name, f) => (name, ensureSym(f(JSLiteralSelf(map)))) }
    evalArgs.foreach(map += _)
    JSLiteralDef(evalArgs)
  }
}

trait JSGenLiteral extends JSGenBase {
  val IR: JSLiteralExp
  import IR._

  def getMember(receiver : Exp[Any], member: String) = receiver match {
    case JSLiteralSelf(map) => quote(map.get(member).get)
    case _ => quote(receiver) + "." + member
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case JSLiteralDef(members) => emitValDef(sym,
      members.map({case (name, value) => "'" + name + "' : " + quote(value)}).mkString("{", ",", "}"))
    case Member(value) => emitValDef(sym, quote(value))
    case MemberCall(receiver, method, args) =>  emitValDef(sym,
      getMember(receiver, method) + args.map(quote).mkString("(", ",", ")"))
    case MemberSelect(receiver, field) => emitValDef(sym,
      getMember(receiver, field))
    case _ => super.emitNode(sym, rhs)
  }
}
