package scala.js.gen.js

import scala.js.exp.OptionOpsExp
import scala.js.gen.QuoteGen

trait GenOptionOps extends GenEffect with QuoteGen {
  val IR: OptionOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OptionForeach(o, a, block, bound) =>
      stream.println("if (" + quote(o) + " !== null) {")
      if (bound) emitValDef(a, quote(o))
      emitBlock(block)
      stream.println("}")
      emitValDef(sym, "undefined")
    case OptionMap(o, a, block, bound) =>
      emitValDef(sym, "null")
      stream.println(q"if ($o !== null) {")
      if (bound) emitValDef(a, quote(o))
      emitBlock(block)
      emitAssignment(quote(sym), quote(getBlockResult(block)))
      stream.println("}")
    case OptionFlatMap(o, a, block, bound) =>
      emitValDef(sym, "null")
      stream.println(q"if ($o !== null) {")
      if (bound) emitValDef(a, quote(o))
      emitBlock(block)
      emitAssignment(quote(sym), quote(getBlockResult(block)))
      stream.println("}")
    case OptionIsEmpty(o) =>
      emitValDef(sym, quote(o) + " === null")
    case OptionFold(o, a, n, s, bound) =>
      emitValDef(sym, "undefined")
      stream.println(q"if ($o !== null) {")
      if (bound) emitValDef(a, quote(o))
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