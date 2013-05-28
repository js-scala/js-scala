package scala.js.gen.js

import scala.js.exp.OptionOpsExp

trait GenOptionOps extends GenEffect {
  val IR: OptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OptionForeach(o, a, block) =>
      stream.println("if (" + quote(o) + " !== null) {")
      emitValDef(a, quote(o)) // Ouin
      emitBlock(block)
      stream.println("}")
      emitValDef(sym, "undefined")
    case OptionMap(o, a, block) =>
      emitValDef(a, quote(o))
      stream.println("if (" + quote(a) + " !== null) {")
      emitBlock(block)
      stream.println(quote(a) + " = " + quote(getBlockResult(block)) + ";")
      stream.println("}")
      emitValDef(sym, quote(a))
    case OptionFlatMap(o, a, block) =>
      emitValDef(a, quote(o))
      stream.println("if (" + quote(a) + " !== null) {")
      emitBlock(block)
      stream.println(quote(a) + " = " + quote(getBlockResult(block)) + ";")
      stream.println("}")
      emitValDef(sym, quote(a))
    case OptionIsEmpty(o) =>
      emitValDef(sym, quote(o) + " === null")
    case OptionFold(o, a, n, s) =>
      emitValDef(sym, "undefined")
      emitValDef(a, quote(o))
      stream.println("if (" + quote(a) + " !== null) {")
      emitBlock(s)
      emitAssignment(quote(sym), quote(getBlockResult(s)))
      stream.println("} else {")
      emitBlock(n)
      emitAssignment(quote(sym), quote(getBlockResult(n)))
      stream.println("}")
    case OptionSome(a) =>
      emitValDef(sym, quote(a))
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) = x match {
    case `none` => "null"
    case _ => super.quote(x)
  }
}