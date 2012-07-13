package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait Casts extends Base {
  trait AsRep {
    def as[T: Manifest]: Rep[T]
  }
  implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T: Manifest]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}

trait CastsCheckedExp extends Casts with EffectExp {
  case class Cast[T](x: Rep[_], m: Manifest[T]) extends Def[T]
  override implicit def asRep(x: Rep[_]): AsRep = new AsRep {
    def as[T: Manifest]: Rep[T] = reflectEffect(Cast[T](x, implicitly[Manifest[T]]))
  }
}

trait GenCastChecked extends JSGenEffect {
  val IR: CastsCheckedExp
  import IR._
  
  import scala.reflect._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Cast(x, m) =>
      emitValDef(sym, quote(x))
      conformsCheck(quote(sym), m)
    case _ => super.emitNode(sym, rhs)
  }
  def conformsCheck(v: String, m: Manifest[_]): Unit = m match {
    case m: RefinedManifest[_] =>
      m.fields foreach { case (name, manifest) => conformsFieldCheck(v, name, manifest) }
    case m if m == Manifest.classType(classOf[String]) => ()
    case Manifest.Int => ()
    case m if m.erasure.isArray =>
      stream.println("""if (!(%1$s instanceof Array)) throw "Not an Array: " + %1$s;""".format(v))
      val List(arrayElem) = m.typeArguments
      stream.println("""for (var i=0; i < %1$s; i++) {""".format(v + ".length"))
      conformsCheck(v + "[i]", arrayElem)
      stream.println("}")
    case _ => println("Can't generate check for " + m + " with of type " + m.getClass)
  }
  def conformsFieldCheck(v: String, f: String, m: Manifest[_]): Unit = {
    stream.println("""if (!("%1$s" in %2$s)) throw "%1$s is not defined in " + %2$s;""".format(f, v))
    conformsCheck(v + "." + f, m)
  }

}
