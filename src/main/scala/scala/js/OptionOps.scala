package scala.js

import virtualization.lms.common._

trait OptionOps { this: Base =>
  class OptionOpsCls[+A : Manifest](o: Rep[Option[A]]) {
    def foreach(f: Rep[A] => Rep[Unit]) = option_foreach(o, f)
    def map[B : Manifest](f: Rep[A] => Rep[B]) = option_map(o, f)
    def flatMap[B : Manifest](f: Rep[A] => Rep[Option[B]]) = option_flatMap(o, f)
  }
  implicit def repToOptionOpsCls[A : Manifest](o: Rep[Option[A]]): OptionOpsCls[A] = new OptionOpsCls(o)
  def option_foreach[A : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[Unit]): Rep[Unit]
  def option_map[A : Manifest, B : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[B]): Rep[Option[B]]
  def option_flatMap[A : Manifest, B : Manifest](o: Rep[Option[A]], f: Rep[A] => Rep[Option[B]]): Rep[Option[B]]
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

  case class OptionForeach[A : Manifest](o: Exp[Option[A]], a: Sym[A], block: Block[Unit]) extends Def[Unit]
  case class OptionMap[A, B](o: Exp[Option[A]], a: Sym[A], block: Block[B]) extends Def[Option[B]]
  case class OptionFlatMap[A : Manifest, B](o: Exp[Option[A]], a: Sym[A], block: Block[Option[B]]) extends Def[Option[B]]

  override def syms(e: Any) = e match {
    case OptionForeach(o, _, block) => List(o, block).flatMap(syms)
    case OptionMap(o, _, block) => List(o, block).flatMap(syms)
    case OptionFlatMap(o, _, block) => List(o, block).flatMap(syms)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case OptionForeach(_, a, block) => a :: effectSyms(block)
    case OptionMap(_, a, block) => a :: effectSyms(block)
    case OptionFlatMap(_, a, block) => a :: effectSyms(block)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case OptionForeach(o, _, block) => List(o, block).flatMap(freqNormal)
    case OptionMap(o, _, block) => List(o, block).flatMap(freqNormal)
    case OptionFlatMap(o, _, block) => List(o, block).flatMap(freqNormal)
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
    case _ => super.emitNode(sym, rhs)
  }
}