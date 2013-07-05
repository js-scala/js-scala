package scala.js.gen.js

import scala.js.exp.TraitsExp

trait GenTraits extends GenBase with GenProxy {
  val IR: TraitsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ClassTemplate(parentTemplate, methodTemplates) =>
      stream.println("var " + quote(sym) + " = function() {")
      stream.println("this.$init$()")
      stream.println("}")
      parentTemplate.foreach(pt => stream.println(quote(sym) + ".prototype = " + quote(pt.instance)))
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