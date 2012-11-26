package scala.js

import virtualization.lms.common._

trait OptionOps { this: Base =>
  implicit class OptionOpsCls[+A : Manifest](o: Rep[Option[A]]) {
    def foreach(f: Rep[A] => Rep[Unit]) = option_foreach(o, f)
    def map[B : Manifest](f: Rep[A] => Rep[B]) = option_map(o, f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[Option[B]]) = option_flatMap(o, f)
    def isEmpty = option_isEmpty(o)
    def fold[B : Manifest](none: => Rep[B], some: Rep[A] => Rep[B]) = option_fold(o, none, some)
  }
  def option_foreach[A : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
  def option_map[A : Manifest, B : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[B]): Rep[Option[B]]
  def option_flatMap[A : Manifest, B : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[Option[B]]): Rep[Option[B]]
  def option_isEmpty[A : Manifest](o: Rep[Option[A]]): Rep[Boolean]
  def option_fold[A : Manifest, B : Manifest](o: Rep[Option[A]], none: => Rep[B], some: Rep[A] => Rep[B]): Rep[B]
}

trait OptionOpsExp extends OptionOps with EffectExp {
  def option_foreach[A : Manifest](o: Exp[Option[A]], f: Exp[A] => Exp[Unit]) = {
    val a = fresh[A]
    val block = reifyEffects(f(a))
    reflectEffect(OptionForeach(o, a, block), summarizeEffects(block).star)
  }
  def option_map[A : Manifest, B : Manifest](o: Exp[Option[A]], f: Exp[A] => Exp[B]) = {
    val a = fresh[A]
    val block = reifyEffects(f(a))
    reflectEffect(OptionMap(o, a, block), summarizeEffects(block).star)
  }
  def option_flatMap[A : Manifest, B : Manifest](o: Exp[Option[A]], f: Exp[A] => Exp[Option[B]]) = {
    val a = fresh[A]
    val block = reifyEffects(f(a))
    reflectEffect(OptionFlatMap(o, a, block), summarizeEffects(block).star)
  }
  def option_isEmpty[A : Manifest](o: Exp[Option[A]]) = OptionIsEmpty(o)
  def option_fold[A : Manifest, B : Manifest](o: Exp[Option[A]], none: => Rep[B], some: Rep[A] => Rep[B]) = {
    val a = fresh[A]
    val noneBlock = reifyEffectsHere(none)
    val someBlock = reifyEffects(some(a))
    reflectEffect(OptionFold(o, a, noneBlock, someBlock), summarizeEffects(noneBlock) orElse summarizeEffects(someBlock))
  }

  case class OptionForeach[A](o: Exp[Option[A]], a: Sym[A], block: Block[Unit]) extends Def[Unit]
  case class OptionMap[A, B](o: Exp[Option[A]], a: Sym[A], block: Block[B]) extends Def[Option[B]]
  case class OptionFlatMap[A, B](o: Exp[Option[A]], a: Sym[A], block: Block[Option[B]]) extends Def[Option[B]]
  case class OptionIsEmpty[A](o: Exp[Option[A]]) extends Def[Boolean]
  case class OptionFold[A, B](o: Exp[Option[A]], a: Sym[A], noneBlock: Block[B], someBlock: Block[B]) extends Def[B]

  override def syms(e: Any) = e match {
    case OptionForeach(o, _, block) => List(o, block).flatMap(syms)
    case OptionMap(o, _, block) => List(o, block).flatMap(syms)
    case OptionFlatMap(o, _, block) => List(o, block).flatMap(syms)
    case OptionFold(o, _, b1, b2) => List(o, b1, b2).flatMap(syms)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case OptionForeach(_, a, block) => a :: effectSyms(block)
    case OptionMap(_, a, block) => a :: effectSyms(block)
    case OptionFlatMap(_, a, block) => a :: effectSyms(block)
    case OptionFold(_, a, b1, b2) => a :: effectSyms(b1) ::: effectSyms(b2)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case OptionForeach(o, _, block) => freqNormal(o) ++ freqCold(block)
    case OptionMap(o, _, block) => freqNormal(o) ++ freqCold(block)
    case OptionFlatMap(o, _, block) => freqNormal(o) ++ freqCold(block)
    case OptionFold(o, _, b1, b2) => freqNormal(o) ++ freqCold(b1) ++ freqCold(b2)
    case _ => super.symsFreq(e)
  }

}

trait JSGenOptionOps extends JSGenEffect {
  val IR: EffectExp with OptionOpsExp
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
    case _ => super.emitNode(sym, rhs)
  }
}