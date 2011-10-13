package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait Arrays extends Base {
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOps(a)

  class ArrayOps[T:Manifest](a: Rep[Array[T]]) {
    def apply(i: Rep[Int]) = array_apply(a, i)
    def length = array_length(a)
    def update(i: Rep[Int], x: Rep[T]) = array_update(a, i, x)
    def foreach(block: Rep[T] => Rep[Unit]) = array_foreach(a, block)
    def map[U:Manifest](block: Rep[T] => Rep[U]) = array_map(a, block)
    def flatMap[U:Manifest](block: Rep[T] => Rep[Array[U]]) = array_flatMap(a, block)
    def filter(block: Rep[T] => Rep[Boolean]) = array_filter(a, block)
  }

  def array[T:Manifest](xs: Rep[T]*): Rep[Array[T]]
  def array_apply[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def array_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def array_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  def array_foreach[T:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def array_map[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[U]): Rep[Array[U]]
  def array_flatMap[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Array[U]]): Rep[Array[U]]
  def array_filter[T:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Boolean]): Rep[Array[T]]

  case class Range(a: Rep[Int], b: Rep[Int]) {
    def foreach(block: Rep[Int] => Rep[Unit]) = range_foreach(this, block)
    def map[U:Manifest](block: Rep[Int] => Rep[U]) = range_map(this, block)
    def flatMap[U:Manifest](block: Rep[Int] => Rep[Array[U]]) = range_flatMap(this, block)
    def filter(block: Rep[Int] => Rep[Boolean]) = range_filter(this, block)
  }
  def range(a: Rep[Int], b: Rep[Int]) = Range(a, b)
  def range_foreach(r: Range, block: Rep[Int] => Rep[Unit]): Rep[Unit]
  def range_map[U:Manifest](r: Range, block: Rep[Int] => Rep[U]): Rep[Array[U]]
  def range_flatMap[U:Manifest](r: Range, block: Rep[Int] => Rep[Array[U]]): Rep[Array[U]]
  def range_filter(r: Range, block: Rep[Int] => Rep[Boolean]): Rep[Array[Int]]
}

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayLiteral[T:Manifest](xs: List[Rep[T]]) extends Def[Array[T]]
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], i: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]
  case class ArrayForeach[T:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[Unit]) extends Def[Unit]
  case class ArrayMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[U]) extends Def[Array[U]]
  case class ArrayFlatMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[Array[U]]) extends Def[Array[U]]
  case class ArrayFilter[T:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[Boolean]) extends Def[Array[T]]
  case class RangeForeach(r: Range, i: Sym[Int], block: Exp[Unit]) extends Def[Unit]
  case class RangeMap[U:Manifest](r: Range, i: Sym[Int], block: Exp[U]) extends Def[Array[U]]
  case class RangeFlatMap[U:Manifest](r: Range, i: Sym[Int], block: Exp[Array[U]]) extends Def[Array[U]]
  case class RangeFilter(r: Range, i: Sym[Int], block: Exp[Boolean]) extends Def[Array[Int]]

  def array[T:Manifest](xs: Exp[T]*) = reflectEffect(ArrayLiteral(xs.toList), Alloc())
  def array_apply[T:Manifest](a: Exp[Array[T]], i: Exp[Int]) = ArrayApply(a, i)
  def array_length[T:Manifest](a: Exp[Array[T]]) = ArrayLength(a)
  def array_update[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T]) = reflectEffect(ArrayUpdate(a, i, x))
  def array_foreach[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Unit]) = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayForeach(a, x, b), summarizeEffects(b).star)
  }
  def array_map[T:Manifest,U:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[U]) = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayMap(a, x, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def array_flatMap[T:Manifest,U:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Array[U]]) = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayFlatMap(a, x, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def array_filter[T:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Boolean]) = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayFilter(a, x, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def range_foreach(r: Range, block: Rep[Int] => Rep[Unit]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeForeach(r, i, b), summarizeEffects(b).star)
  }
  def range_map[U:Manifest](r: Range, block: Rep[Int] => Rep[U]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeMap(r, i, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def range_flatMap[U:Manifest](r: Range, block: Rep[Int] => Rep[Array[U]]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeFlatMap(r, i, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def range_filter(r: Range, block: Rep[Int] => Rep[Boolean]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeFilter(r, i, b), Alloc() andAlso summarizeEffects(b).star)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case ArrayMap(a, x, body) => syms(a):::syms(body)
    case ArrayFlatMap(a, x, body) => syms(a):::syms(body)
    case ArrayFilter(a, x, body) => syms(a):::syms(body)
    case Range(a, b) => syms(a):::syms(b)
    case RangeForeach(r, i, body) => syms(r):::syms(body)
    case RangeMap(r, i, body) => syms(r):::syms(body)
    case RangeFlatMap(r, i, body) => syms(r):::syms(body)
    case RangeFilter(r, i, body) => syms(r):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case ArrayMap(a, x, body) => x :: effectSyms(body)
    case ArrayFlatMap(a, x, body) => x :: effectSyms(body)
    case ArrayFilter(a, x, body) => x :: effectSyms(body)
    case RangeForeach(r, i, body) => i :: effectSyms(body)
    case RangeMap(r, i, body) => i :: effectSyms(body)
    case RangeFlatMap(r, i, body) => i :: effectSyms(body)
    case RangeFilter(r, i, body) => i :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayFlatMap(a, x, body) => freqHot(a):::freqHot(body)
    case ArrayFilter(a, x, body) => freqNormal(a):::freqHot(body)
    case Range(a, b) => freqNormal(a):::freqHot(b)
    case RangeForeach(r, i, body) => symsFreq(r):::freqHot(body)
    case RangeMap(r, i, body) => symsFreq(r):::freqHot(body)
    case RangeFlatMap(r, i, body) => symsFreq(r):::freqHot(body)
    case RangeFilter(r, i, body) => symsFreq(r):::freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait JSGenArrays extends JSGenEffect {
  val IR: ArraysExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ArrayLiteral(xs) => emitValDef(sym, xs.map(quote).mkString("[", ", ", "]"))
    case ArrayApply(a, i) => emitValDef(sym, quote(a) + "[" + quote(i) + "]")
    case ArrayLength(a) => emitValDef(sym, quote(a) + ".length")
    case ArrayUpdate(a, i, x) => emitValDef(sym, quote(a) + "[" + quote(i) + "] = " + quote(x))
    case ArrayForeach(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".forEach(")
      stream.println("function(" + quote(x) + ",i_,a_){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case ArrayMap(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case ArrayFlatMap(a, x, block) =>
      emitValDef(sym, "[]")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0; " + quote(i) + "< " + quote(a) + ".length; " + quote(i) + "++){")
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length, 0].concat((function(" + quote(x) + "){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})(" + quote(a) + "[" + quote(i) + "])))")
      stream.println("}")
    case ArrayFilter(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".filter(")
      stream.println("function(" + quote(x) + ",i_,a_){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case RangeForeach(Range(a, b), i, block) =>
      emitValDef(sym, "undefined")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println("}")
    case RangeMap(Range(a, b), i, block) =>
      emitValDef(sym, "[]")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println(quote(sym) + "[" + quote(i) + "]=" + quote(getBlockResult(block)))
      stream.println("}")
    case RangeFlatMap(Range(a, b), i, block) =>
      emitValDef(sym, "[]")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length,0].concat(" + quote(getBlockResult(block)) + "))")
      stream.println("}")
    case RangeFilter(Range(a, b), i, block) =>
      emitValDef(sym, "[]")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println("if (" + quote(getBlockResult(block)) + ") {")
      stream.println(quote(sym) + "[" + quote(sym) + ".length]=" + quote(getBlockResult(block)))
      stream.println("}}")
    case _ => super.emitNode(sym, rhs)
  }
}
