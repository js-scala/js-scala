package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}

import java.io.PrintWriter

trait JSProxyBase extends Base {
  def repProxy[T<:AnyRef](x: Rep[T])(implicit m: Manifest[T]): T
}

trait JSProxyExp extends JSProxyBase with BaseExp with EffectExp {

  case class MethodCall[T](receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[T]
  case class SuperMethodCall[T](receiver: Exp[Any], parentConstructor: Option[Exp[Any]], method: String, args: List[Exp[Any]]) extends Def[T]
  case class FieldAccess[T](receiver: Exp[Any], field: String) extends Def[T]
  case class FieldUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]

  def repProxy[T<:AnyRef](x: Rep[T])(implicit m: Manifest[T]): T = {
    proxy[T](x, None, null)(m)
  }

  def proxyTrait[T<:AnyRef](x: Rep[T], parentConstructor: Option[Rep[Any]], outer: AnyRef)(implicit m: Manifest[T]): T = {
    proxy[T](x, parentConstructor, outer)(m)
  }

  def proxy[T<:AnyRef](x: Rep[T], parentConstructor: Option[Rep[Any]], outer: AnyRef)(implicit m: Manifest[T]): T = {
    val clazz = m.erasure
    val handler = new JSInvocationHandler(x, parentConstructor, outer)
    val proxy = jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), handler)
    proxy.asInstanceOf[T]
  }

  class JSInvocationHandler(receiver: Exp[Any], parentConstructor: Option[Rep[Any]], outer: AnyRef) extends jreflect.InvocationHandler with java.io.Serializable {
    private val fieldUpdateMarker = "_$eq"
    private def isFieldUpdateMethod(name: String) = name.endsWith(fieldUpdateMarker)
    private def fieldFromUpdateMethod(name: String) = name.slice(0, name.length - fieldUpdateMarker.length)
    private def updateMethodFromField(name: String) = name + fieldUpdateMarker

    def invoke(proxy: AnyRef, m: jreflect.Method, args: Array[AnyRef]): AnyRef = {
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

      if (m.getName.endsWith("$$$outer")) outer
      else if (m.getName.contains("$$super$")) {
	val methodName = m.getName.slice(m.getName.indexOf("$$super$") + "$$super$".length, m.getName.length)
	reflectEffect(SuperMethodCall[AnyRef](receiver, parentConstructor, methodName, args_.toList)) : Exp[Any]
      // We use reflectEffect for field access to ensure that reads
      // are serialized with respect to updates.  TODO: Could we use
      // something like reflectMutable and reflectWrite to achieve a
      // finer-granularity? We will need a similar solution for
      // reified new with vars and for dynamic select.
      } else if (isFieldAccess) reflectEffect(FieldAccess[AnyRef](receiver, m.getName)) : Exp[Any]
      else if (isFieldUpdate) reflectEffect(FieldUpdate(receiver, fieldFromUpdateMethod(m.getName), args_(0))) : Exp[Any]
      else reflectEffect(MethodCall[AnyRef](receiver, m.getName, args_.toList)) : Exp[Any]
    }
  }

}

trait JSGenProxy extends JSGenBase with JSGenEffect {
  val IR: JSProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MethodCall(receiver, method, args) =>  emitValDef(sym,
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case SuperMethodCall(receiver, parentConstructor, method, args) =>  emitValDef(sym,
      (parentConstructor match { case Some(parentConstructor) => quote(parentConstructor); case None => "Object" }) +
      ".prototype." + method + ".call" + (receiver::args).map(quote).mkString("(", ",", ")"))
    case FieldAccess(receiver, field) =>  emitValDef(sym,
      quote(receiver) + "." + field)
    case FieldUpdate(receiver, field, value) =>  emitValDef(sym,
      quote(receiver) + "." + field + " = " + quote(value))
    case _ => super.emitNode(sym, rhs)
  }
}
