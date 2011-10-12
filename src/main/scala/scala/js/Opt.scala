package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait JSCodegenOpt extends JSNestedCodegen {
  import IR._

  val defUseMap : scala.collection.mutable.Map[Sym[_], Int] = new scala.collection.mutable.HashMap
  def addDefUse(sym: Sym[_]) = defUseMap.update(sym, defUseMap.getOrElse(sym, 0) + 1)
  def defUse(sym: Sym[_]) = defUseMap.getOrElse(sym, 0)

  def buildDefUse(body: Exp[Any]): Unit = {
    defUseMap.clear()
    
    refSyms(getBlockResult(body)).foreach(addDefUse)
    for (TP(_, rhs) <- buildScheduleForResult(body)) {
      for (sym <- refSyms(rhs)) {
	addDefUse(sym)
      }
    }
  }

  def refSyms(e: Any): List[Sym[Any]] = e match {
    case Reify(x, u, es) => readSyms(x)
    case _ => readSyms(e)
  }

  override def emitSourceAnyArity(args: List[Exp[Any]], body: Exp[Any], methName: String, stream: PrintWriter): Unit = {
    buildDefUse(body)
    super.emitSourceAnyArity(args, body, methName, stream)
  }

  override def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    if (defUse(sym) == 0) stream.println(rhs)
    else super.emitValDef(sym, rhs)
  }
}

trait NumericOpsExpOpt extends NumericOpsExp {

  override def numeric_plus[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.plus(x, y))
      case (x, Const(y)) if y == t.zero  => x
      case (Const(x), y) if x == t.zero => y
      case _ => super.numeric_plus(x, y)
    }
  }

  override def numeric_minus[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      case (Const(x), Const(y)) => Const(t.minus(x, y))
      case (x, Const(y)) if y == t.zero  => x
      case _ => super.numeric_minus(x, y)
    }
  }

  override def numeric_times[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
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

  override def numeric_divide[T:Numeric:Manifest](x: Exp[T], y: Exp[T]) = {
    val t = implicitly[Numeric[T]]
    (x, y) match {
      // case (Const(x), Const(y)) => Const(x / y)
      case (x, Const(y)) if y == t.zero => x
      case _ => super.numeric_divide(x, y)
    }
  }
}
