import scala.virtualization.lms.common._

trait NumericOpsExpOpt extends NumericOpsExp {

  override def numeric_plus[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.plus(x, y))
      //case (x, Const(t.zero) | Const(t.negate(t.zero))) => x
      //case (Const(t.zero) | Const(t.negate(t.zero)), y) => y
      case _ => super.numeric_plus(x, y)
    }
  }

  override def numeric_minus[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.minus(x, y))
      //case (x, Const(t.zero) | Const(t.negate(t.zero))) => x
      case _ => super.numeric_minus(x, y)
    }
  }

  override def numeric_times[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.times(x,y))
      //case (x, Const(t.one)) => x
      //case (Const(t.one), y) => y
      //case (x, Const(t.zero) | Const(t.negate(t.zero))) => Const(t.zero)
      //case ((Const(t.zero) | Const(t.negate(t.zero))), y) => Const(t.zero)
      case _ => super.numeric_times(x, y)
    }
  }

  override def numeric_divide[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      // case (Const(x), Const(y)) => Const(x / y)
      //case (x, Const(t.one)) => x
      case _ => super.numeric_divide(x, y)
    }
  }
}
