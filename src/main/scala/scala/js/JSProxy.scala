package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}

import java.io.PrintWriter

trait JSProxyBase extends Base {
  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops
}

trait JSProxyExp extends JSProxyBase with BaseExp with EffectExp {

  case class MethodCall[T](receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[T]
  case class FieldAccess[T](receiver: Exp[Any], field: String) extends Def[T]
  case class FieldUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]

  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops = {
    val clazz = m.erasure
    val handler = new JSInvocationHandler(x)
    val proxy = jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), handler)
    proxy.asInstanceOf[Ops]
  }

  class JSInvocationHandler(receiver: Exp[Any]) extends jreflect.InvocationHandler {
    private val fieldUpdateMarker = "_$eq"
    private def isFieldUpdateMethod(name: String) = name.endsWith(fieldUpdateMarker)
    private def fieldFromUpdateMethod(name: String) = name.slice(0, name.length - fieldUpdateMarker.length)
    private def updateMethodFromField(name: String) = name + fieldUpdateMarker

    def invoke(proxy: AnyRef, m: jreflect.Method, args: Array[AnyRef]): Exp[Any] = {
      //TODO: Make a check when constructing proxy, not when executing it. Also, check using
      //reflection by enumerating all methods and checking their signatures
      assert(args == null || args.forall(_.isInstanceOf[Exp[_]]), "At the moment only Exps can be passed as arguments.")
      val args_ : Array[Exp[Any]] = if (args == null) Array.empty else args.map(_.asInstanceOf[Exp[Any]])

      // For now, we can only detect field access for vars, as we rely
      // on the existence of the update method.  For vals, Java
      // reflection gives us no way to distinguish abstract vals from
      // 0-argument methods.
      def isFieldAccess: Boolean = {
	if (args != null) return false

	try {
	  m.getDeclaringClass.getMethod(updateMethodFromField(m.getName), m.getReturnType)
	  return true
	} catch {
	  case _ : NoSuchMethodException => return false
	}
      }

      def isFieldUpdate: Boolean =  isFieldUpdateMethod(m.getName) && args_.length == 1

      // We use reflectEffect for field access to ensure that reads
      // are serialized with respect to updates.  TODO: Could we use
      // something like reflectMutable and reflectWrite to achieve a
      // finer-granularity? We will need a similar solution for
      // reified new with vars and for dynamic select.
      if (isFieldAccess) reflectEffect(FieldAccess[AnyRef](receiver, m.getName))
      else if (isFieldUpdate) reflectEffect(FieldUpdate(receiver, fieldFromUpdateMethod(m.getName), args_(0)))
      else reflectEffect(MethodCall[AnyRef](receiver, m.getName, args_.toList))
    }
  }

}

trait JSGenProxy extends JSGenBase with JSGenEffect {
  val IR: JSProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MethodCall(receiver, method, args) =>  emitValDef(sym,
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case FieldAccess(receiver, field) =>  emitValDef(sym,
      quote(receiver) + "." + field)
    case FieldUpdate(receiver, field, value) =>  emitValDef(sym,
      quote(receiver) + "." + field + " = " + quote(value))
    case _ => super.emitNode(sym, rhs)
  }
}
