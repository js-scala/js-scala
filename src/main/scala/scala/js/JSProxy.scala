package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}

import java.io.PrintWriter

trait JSProxyBase extends Base {
  def repProxy[T<:AnyRef](x: Rep[T])(implicit m: Manifest[T]): T
}

trait JSProxyExp extends JSProxyBase with JSCommonProxyExp {

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

  private class JSInvocationHandler(receiver: Exp[Any], parentConstructor: Option[Rep[Any]], outer: AnyRef) extends jreflect.InvocationHandler with java.io.Serializable {
    def invoke(proxy: AnyRef, m: jreflect.Method, args: Array[AnyRef]): AnyRef = {
      val args_ = checkArgs(args)

      if (m.getName.endsWith("$$$outer")) outer
      else if (m.getName.contains("$$super$")) {
	val methodName = m.getName.slice(m.getName.indexOf("$$super$") + "$$super$".length, m.getName.length)
	reflectEffect(SuperMethodCall[AnyRef](receiver, parentConstructor, methodName, args_.toList)) : Exp[Any]
      // We use reflectEffect for field access to ensure that reads
      // are serialized with respect to updates.  TODO: Could we use
      // something like reflectMutable and reflectWrite to achieve a
      // finer-granularity? We will need a similar solution for
      // reified new with vars and for dynamic select.
      } else if (isFieldAccess(args_, m)) reflectEffect(FieldAccess[AnyRef](receiver, m.getName)) : Exp[Any]
      else if (isFieldUpdate(args_, m)) reflectEffect(FieldUpdate(receiver, fieldFromUpdateMethod(m.getName), args_(0))) : Exp[Any]
      else reflectEffect(MethodCall[AnyRef](receiver, m.getName, args_.toList)) : Exp[Any]
    }
  }

}

trait JSGenProxy extends JSGenCommonProxy {
  val IR: JSProxyExp
  import IR._
}
