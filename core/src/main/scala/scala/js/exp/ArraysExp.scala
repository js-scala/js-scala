package scala.js.exp

import scala.js.language.Arrays
import scala.virtualization.lms.common.EffectExp

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayLiteral[T:Manifest](xs: List[Exp[T]]) extends Def[Array[T]]
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], i: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]
  case class ArrayForeach[T:Manifest](a: Exp[Array[T]], x: Sym[T], block: Block[Unit]) extends Def[Unit]
  case class ArrayMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Block[U]) extends Def[Array[U]]
  case class ArrayFlatMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Block[Array[U]]) extends Def[Array[U]]
  case class ArrayFilter[T:Manifest](a: Exp[Array[T]], x: Sym[T], block: Block[Boolean]) extends Def[Array[T]]
  case class ArrayJoin[T:Manifest](a: Exp[Array[T]], s: Exp[String]) extends Def[String]
  case class ArrayToList[T](a: Exp[Array[T]]) extends Def[List[T]]
  case class RangeForeach(r: Range, i: Sym[Int], block: Block[Unit]) extends Def[Unit]
  case class RangeMap[U:Manifest](r: Range, i: Sym[Int], block: Block[U]) extends Def[Array[U]]
  case class RangeFlatMap[U:Manifest](r: Range, i: Sym[Int], block: Block[Array[U]]) extends Def[Array[U]]
  case class RangeFilter(r: Range, i: Sym[Int], block: Block[Boolean]) extends Def[Array[Int]]

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
  def array_join[T:Manifest](a: Exp[Array[T]], s: Exp[String]) = ArrayJoin(a, s)
  override def array_tolist[T : Manifest](a: Exp[Array[T]]) = ArrayToList(a)
  def range_foreach(r: Range, block: Exp[Int] => Exp[Unit]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeForeach(r, i, b), summarizeEffects(b).star)
  }
  def range_map[U:Manifest](r: Range, block: Exp[Int] => Exp[U]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeMap(r, i, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def range_flatMap[U:Manifest](r: Range, block: Exp[Int] => Exp[Array[U]]) = {
    val i = fresh[Int]
    val b = reifyEffects(block(i))
    reflectEffect(RangeFlatMap(r, i, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def range_filter(r: Range, block: Exp[Int] => Exp[Boolean]) = {
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