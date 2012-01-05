package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}
import javassist._
import javassist.util.proxy._

import java.io.PrintWriter

trait JSClassProxyBase extends Base {
  def repClassProxy[T<:AnyRef](x: Rep[T], outer: AnyRef)(implicit m: Manifest[T]): T
}

trait JSClassProxyExp extends JSClassProxyBase with BaseExp with EffectExp {

  case class MethodCall[T](receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[T]
  case class SuperMethodCall[T](receiver: Exp[Any], parentConstructor: Option[Exp[Any]], method: String, args: List[Exp[Any]]) extends Def[T]
  case class FieldAccess[T](receiver: Exp[Any], field: String) extends Def[T]
  case class FieldUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]

  def repClassProxy[T<:AnyRef](x: Rep[T], outer: AnyRef)(implicit m: Manifest[T]): T = {
    val clazz = m.erasure
    val classProxy = repMasqueradeProxy(clazz, x, None, outer, List[String]())
    classProxy.asInstanceOf[T]
  }

  def repMasqueradeProxy(clazz: Class[_], x: Rep[_], parentConstructor: Option[Rep[Any]], outer: AnyRef, notHandledMethodNames: List[String]): AnyRef = {
    val factory = new ProxyFactory()
    factory.setSuperclass(clazz)
    factory.setFilter(
      new MethodFilter() {
        override def isHandled(method: jreflect.Method) =
          !notHandledMethodNames.contains(method.getName)
      })
    val handler = new JSInvocationHandler(x, parentConstructor, outer)

    val constructor = (clazz.getDeclaredConstructors())(0)
    val constructorParams = constructor.getParameterTypes()
    val constructorArgs = constructorParams.map(clazz => null: AnyRef)
    constructorArgs(0) = outer
    val classProxy = factory.create(constructorParams, constructorArgs, handler)
    classProxy
  }

  private val fieldUpdateMarker = "_$eq"
  def isFieldUpdateMethod(name: String) = name.endsWith(fieldUpdateMarker)
  def fieldFromUpdateMethod(name: String) = name.slice(0, name.length - fieldUpdateMarker.length)
  def updateMethodFromField(name: String) = name + fieldUpdateMarker

  class JSInvocationHandler(receiver: Exp[Any], parentConstructor: Option[Rep[Any]], outer: AnyRef) extends MethodHandler with java.io.Serializable {
    def invoke(classProxy: AnyRef, m: jreflect.Method, proceed: jreflect.Method, args: Array[AnyRef]): AnyRef = {
      def checkArgs(args: Array[AnyRef]): Array[Exp[Any]] = {
        //TODO: Make a check when constructing classProxy, not when executing it. Also, check using
        //reflection by enumerating all methods and checking their signatures
        assert(args == null || args.forall(_.isInstanceOf[Exp[_]]), "At the moment only Exps can be passed as arguments.")
        if (args == null) Array.empty else args.map(_.asInstanceOf[Exp[Any]])
      }

      if (m.getName == "$super$") {
        val methodName = args(0).asInstanceOf[String]
        val actualArgs = args(1).asInstanceOf[Array[AnyRef]]
        val methodArgs = checkArgs(actualArgs).toList
	return reflectEffect(SuperMethodCall[AnyRef](receiver, parentConstructor, methodName, methodArgs)) : Exp[Any]
      }

      val args_ = checkArgs(args)

      // For now, we can only detect field access for vars, as we rely
      // on the existence of the update method.  For vals, Java
      // reflection gives us no way to distinguish abstract vals from
      // 0-argument methods.
      def isFieldAccess: Boolean = {
	if (!args_.isEmpty) return false

	try {
	  m.getDeclaringClass.getMethod(updateMethodFromField(m.getName), m.getReturnType)
	  return true
	} catch {
	  case _ : NoSuchMethodException => return false
	}
      }

      def isFieldUpdate: Boolean =  isFieldUpdateMethod(m.getName) && args_.length == 1

      if (m.getName.endsWith("$$$outer")) outer
      // We use reflectEffect for field access to ensure that reads
      // are serialized with respect to updates.  TODO: Could we use
      // something like reflectMutable and reflectWrite to achieve a
      // finer-granularity? We will need a similar solution for
      // reified new with vars and for dynamic select.
      else if (isFieldAccess) reflectEffect(FieldAccess[AnyRef](receiver, m.getName)) : Exp[Any]
      else if (isFieldUpdate) reflectEffect(FieldUpdate(receiver, fieldFromUpdateMethod(m.getName), args_(0))) : Exp[Any]
      else reflectEffect(MethodCall[AnyRef](receiver, m.getName, args_.toList)) : Exp[Any]
    }
  }

}

trait JSGenClassProxy extends JSGenBase with JSGenEffect {
  val IR: JSClassProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
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
