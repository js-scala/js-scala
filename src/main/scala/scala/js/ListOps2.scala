package scala.js

import virtualization.lms.common.{ScalaGenEffect, EffectExp, Base}

trait ListOps2 { this: Base =>
  implicit class ListOps2[A](l: Rep[List[A]]) {
    def mkString2(sep: Rep[String]) = list_mkString2(l, sep)
    def foreach(f: Rep[A] => Rep[Unit])(implicit ev: Manifest[A]) = list_foreach(l, f)
    def foreachWithIndex(f: (Rep[A], Rep[Int]) => Rep[Unit])(implicit ev: Manifest[A]) = list_foreachWithIndex(l, f)
    def size = list_size(l)
  }
  def list_mkString2[A](l: Rep[List[A]], sep: Rep[String]): Rep[String]
  def list_foreach[A : Manifest](l: Rep[List[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
  def list_foreachWithIndex[A : Manifest](l: Rep[List[A]], f: (Rep[A], Rep[Int]) => Rep[Unit]): Rep[Unit]
  def list_size[A](l: Rep[List[A]]): Rep[Int]
}

trait ListOps2Exp extends ListOps2 with EffectExp {
  def list_mkString2[A](l: Exp[List[A]], sep: Exp[String]) = ListMkString2(l, sep)
  def list_foreach[A : Manifest](l: Exp[List[A]], f: Exp[A] => Exp[Unit]) = {
    val a = fresh[A]
    val block = reifyEffects(f(a))
    reflectEffect(ListForeach(l, a, block))
  }
  def list_foreachWithIndex[A : Manifest](l: Rep[List[A]], f: (Exp[A], Exp[Int]) => Exp[Unit]) = {
    val a = fresh[A]
    val i = fresh[Int]
    val block = reifyEffects(f(a, i))
    reflectEffect(ListForeachWithIndex(l, a, i, block))
  }
  def list_size[A](l: Rep[List[A]]) = ListSize(l)

  case class ListMkString2[A](l: Exp[List[A]], sep: Exp[String]) extends Def[String]
  case class ListForeach[A](l: Exp[List[A]], a: Sym[A], b: Block[Unit]) extends Def[Unit]
  case class ListForeachWithIndex[A](l: Exp[List[A]], a: Sym[A], i: Sym[Int], b: Block[Unit]) extends Def[Unit]
  case class ListSize[A](l: Rep[List[A]]) extends Def[Int]

  override def syms(e: Any) = e match {
    case ListForeach(l, _, b) => List(l, b).flatMap(syms)
    case ListForeachWithIndex(l, _, _, b) => List(l, b).flatMap(syms)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case ListForeach(_, a, b) => a :: effectSyms(b)
    case ListForeachWithIndex(_, a, i, b) => a :: i :: effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case ListForeach(l, _, b) => freqNormal(l) ++ freqHot(b)
    case ListForeachWithIndex(l, _, _, b) => freqNormal(l) ++ freqHot(b)
    case _ => super.symsFreq(e)
  }
}

trait JSGenListOps2 extends JSGenEffect with QuoteGen {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListMkString2(l, sep) =>
      emitValDef(sym, q"$l.join($sep)")
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

trait ScalaGenListOps2 extends ScalaGenEffect with QuoteGen {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListMkString2(l, sep) =>
      emitValDef(sym, q"$l.mkString($sep)")
    case ListForeachWithIndex(l, a, i, b) =>
      stream.println(q"var $sym = $l.zipWithIndex.foreach { case ($a, $i) =>")
      emitBlock(b)
      stream.println("}")
    case ListSize(l) =>
      emitValDef(sym, q"$l.size")
    case _ => super.emitNode(sym, rhs)
  }
}