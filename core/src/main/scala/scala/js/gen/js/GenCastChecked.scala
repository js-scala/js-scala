package scala.js.gen.js

import scala.js.exp.CastsCheckedExp


trait GenCastChecked extends GenEffect {
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
    case m if m.runtimeClass.isArray =>
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
