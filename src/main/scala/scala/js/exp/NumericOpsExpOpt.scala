package scala.js.exp

import scala.reflect.SourceContext
import scala.virtualization.lms.common.NumericOpsExp

trait NumericOpsExpOpt extends NumericOpsExp {

  override def numeric_plus[T:Numeric:Manifest](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.plus(x, y))
      case (x, Const(y)) if y == t.zero  => x
      case (Const(x), y) if x == t.zero => y
      case _ => super.numeric_plus(x, y)
    }
  }

  override def numeric_minus[T:Numeric:Manifest](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.minus(x, y))
      case (x, Const(y)) if y == t.zero  => x
      case _ => super.numeric_minus(x, y)
    }
  }

  override def numeric_times[T:Numeric:Manifest](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.times(x,y))
      case (x, Const(y)) if y == t.one => x
      case (Const(x), y) if x == t.one => y
      case (x, Const(y)) if y == t.zero  => Const(t.zero)
      case (Const(x), y) if x == t.zero => Const(t.zero)
      case _ => super.numeric_times(x, y)
    }
  }

  override def numeric_divide[T:Numeric:Manifest](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      // case (Const(x), Const(y)) => Const(x / y)
      case (x, Const(y)) if y == t.zero => x
      case _ => super.numeric_divide(x, y)
    }
  }
}
