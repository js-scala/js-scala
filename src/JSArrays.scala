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
  }

  def array[T:Manifest](xs: Rep[T]*): Rep[Array[T]]
  def array_apply[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def array_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def array_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  def array_foreach[T:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def array_map[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[U]): Rep[Array[U]]
  def array_flatMap[T:Manifest,U:Manifest](a: Rep[Array[T]], block: Rep[T] => Rep[Array[U]]): Rep[Array[U]]
}

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayLiteral[T:Manifest](xs: List[Rep[T]]) extends Def[Array[T]]
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], i: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]
  case class ArrayForeach[T:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[Unit]) extends Def[Unit]
  case class ArrayMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[U]) extends Def[Array[U]]
  case class ArrayFlatMap[T:Manifest,U:Manifest](a: Exp[Array[T]], x: Sym[T], block: Exp[Array[U]]) extends Def[Array[U]]

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
    reflectEffect(ArrayMap(a, x, b), Alloc() orElse summarizeEffects(b).star)
  }
  def array_flatMap[T:Manifest,U:Manifest](a: Exp[Array[T]], block: Exp[T] => Exp[Array[U]]) = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayFlatMap(a, x, b), Alloc() orElse summarizeEffects(b).star)
  }


  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => syms(a):::syms(body)
    case ArrayMap(a, x, body) => syms(a):::syms(body)
    case ArrayFlatMap(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayForeach(a, x, body) => x :: effectSyms(body)
    case ArrayMap(a, x, body) => x :: effectSyms(body)
    case ArrayFlatMap(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayForeach(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayMap(a, x, body) => freqNormal(a):::freqHot(body)
    case ArrayFlatMap(a, x, body) => freqHot(a):::freqHot(body)
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
      stream.println(quote(getBlockResult(block)))
      stream.println("})")
    case ArrayMap(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("})")
    case ArrayFlatMap(a, x, block) =>
      emitValDef(sym, "[]")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0; " + quote(i) + "< " + quote(a) + ".length; " + quote(i) + "++){")
      stream.println(quote(sym) + ".slice.apply(" + quote(sym) + ", [-1, 0].concat((function(" + quote(x) + "){")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("})(" + quote(a) + "[" + quote(i) + "])))")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
