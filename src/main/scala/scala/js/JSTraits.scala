package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait JSTraits extends JSProxyBase {
  trait Factory[+T] {
    def apply(): Rep[T]
  }
  def register[T<:AnyRef:Manifest](outer: AnyRef): Factory[T]
}

trait JSTraitsExp extends JSTraits with JSProxyExp {
  trait Constructor[T]

  case class MethodTemplate(name: String, params: List[Sym[Any]], body: Exp[Any])

  case class This[+T:Manifest]() extends Exp[T]

  case class ClassTemplate[T:Manifest](methods: List[MethodTemplate]) extends Def[Constructor[T]]
  case class New[T:Manifest](constructor: Exp[Constructor[T]]) extends Def[T]

  override def register[T<:AnyRef:Manifest](outer: AnyRef) = {
    val self = proxyTrait[T](This[T](), outer)

    val m = implicitly[Manifest[T]]
    val traitClazz = m.erasure
    val implClazz = Class.forName(traitClazz.getName + "$class")

    val methods = 
      for (method <- implClazz.getDeclaredMethods.toList)
	yield {
	  val n = method.getParameterTypes.length
	  val params = (1 to (n-1)).toList.map(_ => fresh[Any])
	  val args = (self::params).toArray
	  MethodTemplate(method.getName, params, reifyEffects(method.invoke(null, args: _*).asInstanceOf[Exp[Any]]))
	}

    new Factory[T] {
      override def apply() = reflectEffect(New(ClassTemplate[T](methods)))
    }
  }

  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case MethodTemplate(_, params, body) => syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MethodTemplate(_, params, body) => params.flatMap(syms) ::: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MethodTemplate(_, params, body) => freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait JSGenTraits extends JSGenBase with JSGenProxy {
  val IR: JSTraitsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ClassTemplate(methodTemplates) =>
      emitValDef(sym, "function() { this.$init$(); this }")
      for (MethodTemplate(name, params, body) <- methodTemplates) {
	stream.println(quote(sym) + ".prototype." + name + " = function" + params.map(quote).mkString("(", ",", ")") + " {")
	emitBlock(body)
	stream.println("return " + quote(getBlockResult(body)))
	stream.println("}")
      }
    case New(constructor) =>
      emitValDef(sym, "new " + quote(constructor) + "()")
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case This() => "this"
    case _ => super.quote(x)
  }
}
