package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}

import java.io.PrintWriter

trait JSProxyBase extends Base {
  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops
}

trait JSProxyExp extends JSProxyBase with BaseExp with EffectExp {

  case class MethodCall[T](receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[T]

  def proxyOps[T,Ops<:AnyRef](x: Rep[T])(implicit m: Manifest[Ops]): Ops = {
    val clazz = m.erasure
    val handler = new JSInvocationHandler(x)
    val proxy = jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), handler)
    proxy.asInstanceOf[Ops]
  }

  class JSInvocationHandler(receiver: Exp[Any]) extends jreflect.InvocationHandler {
    def invoke(proxy: AnyRef, m: jreflect.Method, args: Array[AnyRef]): Exp[Any] = {
      //TODO: Make a check when constructing proxy, not when executing it. Also, check using
      //reflection by enumerating all methods and checking their signatures
      val args_ = if (args == null) Array.empty else args
      assert(args_.forall(_.isInstanceOf[Exp[_]]), "At the moment only Exps can be passed as arguments.")
      reflectEffect(MethodCall[AnyRef](receiver, m.getName, args_.map(_.asInstanceOf[Exp[Any]]).toList))
    }
  }

}

trait JSGenProxy extends JSGenBase with JSGenEffect {
  val IR: JSProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case MethodCall(receiver, method, args) =>  emitValDef(sym,
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym, rhs)
  }
}
