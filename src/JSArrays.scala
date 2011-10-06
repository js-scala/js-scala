import scala.virtualization.lms.common._

import java.io.PrintWriter

trait Arrays extends Base {
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOps(a)

  class ArrayOps[T:Manifest](a: Rep[Array[T]]) {
    def apply(i: Rep[Int]) = array_apply(a, i)
    def length = array_length(a)
    def update(i: Rep[Int], x: Rep[T]) = array_update(a, i, x)
  }

  def array[T:Manifest](xs: Rep[T]*): Rep[Array[T]]
  def array_apply[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T]
  def array_length[T:Manifest](a: Rep[Array[T]]): Rep[Int]
  def array_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]): Rep[Unit]
  
}

trait ArraysExp extends Arrays with EffectExp {
  case class ArrayLiteral[T:Manifest](xs: List[Rep[T]]) extends Def[Array[T]]
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], i: Exp[Int]) extends Def[T]
  case class ArrayLength[T:Manifest](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], i: Exp[Int], x: Exp[T]) extends Def[Unit]

  def array[T:Manifest](xs: Rep[T]*) = reflectEffect(ArrayLiteral(xs.toList), Alloc())
  def array_apply[T:Manifest](a: Rep[Array[T]], i: Rep[Int]) = ArrayApply(a, i)
  def array_length[T:Manifest](a: Rep[Array[T]]) = ArrayLength(a)
  def array_update[T:Manifest](a: Rep[Array[T]], i: Rep[Int], x: Rep[T]) = reflectEffect(ArrayUpdate(a, i, x))
}

trait JSGenArrays extends JSGenEffect {
  val IR: ArraysExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ArrayLiteral(xs) => emitValDef(sym, xs.map(quote).mkString("[", ", ", "]"))
    case ArrayApply(a, i) => emitValDef(sym, quote(a) + "[" + quote(i) + "]")
    case ArrayLength(a) => emitValDef(sym, quote(a) + ".length")
    case ArrayUpdate(a, i, x) => emitValDef(sym, quote(a) + "[" + quote(i) + "] = " + quote(x))
    case _ => super.emitNode(sym, rhs)
  }
}
