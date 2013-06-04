package scala.js.exp

import scala.js.language.OptionOps
import scala.virtualization.lms.common.EffectExp

trait OptionOpsExp extends OptionOps with EffectExp {

  case object none extends Exp[None.type]
  case class OptionSome[A](a: Exp[A]) extends Def[Option[A]]
  def some[A : Manifest](a: Exp[A]) = OptionSome(a)

  implicit class OptionOpsCls[+A : Manifest](o: Exp[Option[A]]) extends OptionOpsBase[A] {

    private def sym = o match {
      case Sym(n) => (Sym[A](n), false)
      case _ => (fresh[A], true)
    }

    def foreach(f: Exp[A] => Exp[Unit]) = { // TODO Optimize constants
      val (a, bound) = sym
      val block = reifyEffects(f(a))
      reflectEffect(OptionForeach(o, a, block, bound), summarizeEffects(block).star)
    }
    def map[B : Manifest](f: Exp[A] => Exp[B]) = {
      val (a, bound) = sym
      val block = reifyEffects(f(a))
      reflectEffect(OptionMap(o, a, block, bound), summarizeEffects(block).star)
    }
    def flatMap[B : Manifest](f: Exp[A] => Exp[Option[B]]) = {
      val (a, bound) = sym
      val block = reifyEffects(f(a))
      reflectEffect(OptionFlatMap(o, a, block, bound), summarizeEffects(block).star)
    }
    def isEmpty = OptionIsEmpty(o)
    def fold[B : Manifest](none: => Exp[B], some: Exp[A] => Exp[B]) = {
      val (a, bound) = sym
      val noneBlock = reifyEffectsHere(none)
      val someBlock = reifyEffects(some(a))
      reflectEffect(OptionFold(o, a, noneBlock, someBlock, bound), summarizeEffects(noneBlock) orElse summarizeEffects(someBlock))
    }
    def getOrElse[B >: A : Manifest](default: => Rep[B]) = fold(default, identity)
  }

  case class OptionForeach[A](o: Exp[Option[A]], a: Sym[A], block: Block[Unit], bound: Boolean) extends Def[Unit]
  case class OptionMap[A, B](o: Exp[Option[A]], a: Sym[A], block: Block[B], bound: Boolean) extends Def[Option[B]]
  case class OptionFlatMap[A, B](o: Exp[Option[A]], a: Sym[A], block: Block[Option[B]], bound: Boolean) extends Def[Option[B]]
  case class OptionIsEmpty[A](o: Exp[Option[A]]) extends Def[Boolean]
  case class OptionFold[A, B](o: Exp[Option[A]], a: Sym[A], noneBlock: Block[B], someBlock: Block[B], bound: Boolean) extends Def[B]

  override def syms(e: Any) = e match {
    case OptionForeach(o, _, block, _) => syms(o) ++ syms(block)
    case OptionMap(o, _, block, _) => syms(o) ++ syms(block)
    case OptionFlatMap(o, _, block, _) => syms(o) ++ syms(block)
    case OptionFold(o, _, b1, b2, _) => syms(o) ++ syms(b1) ++ syms(b2)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case OptionForeach(_, a, block, b) => (if (b) List(a) else Nil) ++ effectSyms(block)
    case OptionMap(_, a, block, b) => (if (b) List(a) else Nil) ++ effectSyms(block)
    case OptionFlatMap(_, a, block, b) => (if (b) List(a) else Nil) ++ effectSyms(block)
    case OptionFold(_, a, b1, b2, b) => (if (b) List(a) else Nil) ++ effectSyms(b1) ++ effectSyms(b2)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case OptionForeach(o, _, block, _) => freqNormal(o) ++ freqCold(block)
    case OptionMap(o, _, block, _) => freqNormal(o) ++ freqCold(block)
    case OptionFlatMap(o, _, block, _) => freqNormal(o) ++ freqCold(block)
    case OptionFold(o, _, b1, b2, _) => freqNormal(o) ++ freqCold(b1) ++ freqCold(b2)
    case _ => super.symsFreq(e)
  }

}