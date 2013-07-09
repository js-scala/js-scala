package scala.js.exp

import scala.js.language.ListOps2
import scala.virtualization.lms.common.EffectExp

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