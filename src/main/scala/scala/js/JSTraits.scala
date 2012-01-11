package scala.js

import scala.virtualization.lms.common._

import java.lang.{reflect => jreflect}

import java.io.PrintWriter

trait JSTraits extends JSProxyBase with JSReifiedComponents {
  trait TraitFactory[+T] {
    def apply(): Rep[T]
  }
  def registerTrait[T<:AnyRef:Manifest](outer: AnyRef): TraitFactory[T]
}

trait JSTraitsExp extends JSTraits with JSProxyExp with JSReifiedComponentsExp {
  override def registerTrait[T<:AnyRef:Manifest](outer: AnyRef) = {
    val constructor = registerInternal[T](outer)
    new TraitFactory[T] {
      override def apply() = create[T](constructor)
    }
  }

  private def create[T<:AnyRef:Manifest](constructor: Exp[Constructor[T]]): Exp[T] =
    reflectEffect(New(constructor, Nil))

  private var registered : Map[String, Exp[Constructor[Any]]] = Map()
  private def registerInternal[T<:AnyRef:Manifest](outer: AnyRef) : Exp[Constructor[T]] = {
    val m = implicitly[Manifest[T]]
    val traitClazz = m.erasure
    val key = traitClazz.getName

    registered.get(key) match {
      case Some(constructor) => return constructor.asInstanceOf[Exp[Constructor[T]]]
      case None => ()
    }

    val implClazz = Class.forName(traitClazz.getName + "$class")
    val parents = traitClazz.getInterfaces.filter(_ != implicitly[Manifest[scala.ScalaObject]].erasure)
    assert (parents.length < 2, "Only single inheritance is supported.")
    val parentConstructor = if (parents.length == 0) None else Some(registerInternal[AnyRef](outer)(Manifest.classType(parents(0))))
    val parent = parentConstructor.map(c => ParentTemplate(c, create[AnyRef](c)))

    val self = proxyTrait[T](This[T](), parentConstructor, outer)
    def createMethodTemplate(method: jreflect.Method) = {
      val n = method.getParameterTypes.length
      val params = (1 to (n-1)).toList.map(_ => fresh[Any])
      val args = (self::params).toArray
      MethodTemplate(method.getName, params, reifyEffects(method.invoke(null, args: _*).asInstanceOf[Exp[Any]]))
    }
    val methods = 
      for (method <- implClazz.getDeclaredMethods.toList)
	yield createMethodTemplate(method)

    val constructor = ClassTemplate[T](parent, methods) : Exp[Constructor[T]]
    registered = registered.updated(key, constructor)
    constructor
  }
}

trait JSGenTraits extends JSGenBase with JSGenProxy with JSGenReifiedComponents {
  val IR: JSTraitsExp
  import IR._
}
