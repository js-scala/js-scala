package scala.js.gen.scala

import scala.js.exp.ListOps2Exp
import scala.js.gen.QuoteGen
import scala.virtualization.lms.common.ScalaGenEffect

trait GenListOps2 extends ScalaGenEffect with QuoteGen {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListForeachWithIndex(l, a, i, b) =>
      stream.println(q"var $sym = $l.zipWithIndex.foreach { case ($a, $i) =>")
      emitBlock(b)
      stream.println("}")
    case ListSize(l) =>
      emitValDef(sym, q"$l.size")
    case _ => super.emitNode(sym, rhs)
  }
}