import scala.virtualization.lms.common._

import java.io.PrintWriter
  
trait JSLiteral extends Base with EmbeddedControls {
  type JSLiteral <: Row[Rep]
  override def __new[T, R[x]](args: (String, R[T] => (R[t] forSome{type t}))*): R[T] =
    newJSLiteral(args.map(_.asInstanceOf[(String, Rep[JSLiteral] => (Rep[t] forSome{type t}))]): _*).asInstanceOf[R[T]]
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): Rep[JSLiteral]
  
  abstract class JSLiteralOps {
    def selectDynamic[T](field: String): Rep[T]
  }
  implicit def jsLiteralOps(receiver: Rep[JSLiteral]): JSLiteralOps

  // -- workaround --
  // Since JSLiteral is abstract, it doesn't have a manifest, but one
  // is required by the LMS core framework. For instance, this will be
  // needed when returning a JSLiteral from a function abstraction.
  implicit def jsLiteralManifest[T <: JSLiteral]: Manifest[T] =
    implicitly[Manifest[AnyRef]].asInstanceOf[Manifest[T]]
}

trait JSLiteralExp extends JSLiteral with BaseExp {
  trait JSLiteral extends Row[Rep]

  case class JSLiteralDef(members: List[(String, Exp[Any])]) extends Def[JSLiteral]
  case class MemberSelect(receiver: Exp[Any], field: String) extends Def[Any]
  private class Self(members: Map[String, Exp[JSLiteral] => Exp[Any]]) extends Exp[JSLiteral] {
    import scala.collection.mutable.{Map => MutMap}
    private val pending: MutMap[String, Exp[JSLiteral] => Exp[Any]] = MutMap(members.toSeq: _*)
    private val done: MutMap[String, Exp[Any]] = MutMap.empty
    private def eval(member: String): Exp[Any] = {
      val x = pending(member)(this)
      pending.remove(member)
      done.update(member, x)
      x
    }
    def apply(member: String): Exp[Any] = done.getOrElseUpdate(member, eval(member))
  }
  class SelfOps(receiver: Self) extends JSLiteralOps {
    def selectDynamic[T](field: String): Exp[T] = receiver(field).asInstanceOf[Exp[T]]
  }

  class JSLiteralOpsImpl(val receiver: Exp[JSLiteral]) extends JSLiteralOps {
    def selectDynamic[T](field: String): Exp[T] =
      (MemberSelect(receiver, field): Exp[Any]).asInstanceOf[Exp[T]]
  }
  implicit def jsLiteralOps(receiver: Exp[JSLiteral]): JSLiteralOps = receiver match {
    case receiver: Self => new SelfOps(receiver)
    case receiver =>       new JSLiteralOpsImpl(receiver)
  }
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): Exp[JSLiteral] = {
    val self = new Self(args.toMap)
    val argNames = args.toList.map(_._1)
    val evalArgs = argNames.map(x => x -> self(x))
    JSLiteralDef(evalArgs)
  }
}

trait JSGenLiteral extends JSGenBase {
  val IR: JSLiteralExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case JSLiteralDef(members) => emitValDef(sym,
      members.map({case (name, value) => "'" + name + "' : " + quote(value)}).mkString("{", ",", "}"))
    case MemberSelect(receiver, field) =>
      emitValDef(sym, quote(receiver) + "." + field)
    case _ => super.emitNode(sym, rhs)
  }
}
