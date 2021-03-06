package scala.js.exp

import scala.js.language.Traits

trait TraitsExp extends Traits with ProxyExp {
  trait Constructor[+T]

  case class MethodTemplate(name: String, params: List[Sym[Any]], body: Block[Any])
  case class ParentTemplate(constructor: Exp[Any], instance: Exp[Any])

  case class This[+T:Manifest]() extends Exp[T]

  case class ClassTemplate[T:Manifest](parent: Option[ParentTemplate], methods: List[MethodTemplate]) extends Def[Constructor[T]]
  case class New[T:Manifest](constructor: Exp[Constructor[T]]) extends Def[T]

  override def register[T<:AnyRef:Manifest](outer: AnyRef) = {
    val constructor = registerInternal[T](outer)
    new Factory[T] {
      override def apply() = create[T](constructor)
    }
  }

  private def create[T<:AnyRef:Manifest](constructor: Exp[Constructor[T]]): Exp[T] =
    reflectEffect(New(constructor))

  private var registered : Map[String, Exp[Constructor[Any]]] = Map()
  private def registerInternal[T<:AnyRef:Manifest](outer: AnyRef) : Exp[Constructor[T]] = {
    val m = implicitly[Manifest[T]]
    val traitClazz = m.runtimeClass
    val key = traitClazz.getName

    registered.get(key) match {
      case Some(constructor) => return constructor.asInstanceOf[Exp[Constructor[T]]]
      case None => ()
    }

    val implClazz = Class.forName(traitClazz.getName + "$class")
    val parents = traitClazz.getInterfaces.filter(_ != implicitly[Manifest[AnyRef]].runtimeClass)
    assert (parents.length < 2, "Only single inheritance is supported.")
    val parentConstructor = if (parents.length == 0) None else Some(registerInternal[AnyRef](outer)(Manifest.classType(parents(0))))
    val parent = parentConstructor.map(c => ParentTemplate(c, create[AnyRef](c)))

    val self = proxyTrait[T](This[T](), parentConstructor, outer)
    val methods = 
      for (method <- implClazz.getDeclaredMethods.toList.sortWith(_.getName < _.getName))
      yield {
        val n = method.getParameterTypes.length
        val params = (1 to (n-1)).toList.map(_ => fresh[Any])
        val args = (self::params).toArray
        MethodTemplate(method.getName, params, reifyEffects(method.invoke(null, args: _*).asInstanceOf[Exp[Any]]))
      }

    val constructor = ClassTemplate[T](parent, methods) : Exp[Constructor[T]]
    registered = registered.updated(key, constructor)
    constructor
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
