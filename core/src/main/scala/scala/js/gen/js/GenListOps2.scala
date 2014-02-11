package scala.js.gen.js

import scala.js.exp.ListOps2Exp
import scala.js.gen.QuoteGen

trait GenListOps2 extends GenEffect with QuoteGen {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListForeach(l, a, b) =>
      stream.println(q"$l.forEach(function ($a) {")
      emitBlock(b)
      stream.println("});")
    case ListForeachWithIndex(l, a, i, b) =>
      stream.println(q"$l.forEach(function ($a, $i) {")
      emitBlock(b)
      stream.println("});")
    case ListSize(l) =>
      emitValDef(sym, q"$l.length")
    case _ => super.emitNode(sym, rhs)
  }
}