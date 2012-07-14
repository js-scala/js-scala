package scala.js

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import java.io.PrintWriter
import scala.reflect.SourceContext

trait NestedCodegenOpt extends GenericNestedCodegen with Codegen {
  import IR._

  val defUseMap : scala.collection.mutable.Map[Sym[_], Int] = new scala.collection.mutable.HashMap
  def addDefUse(sym: Sym[_]) = defUseMap.update(sym, defUseMap.getOrElse(sym, 0) + 1)
  def defUse(sym: Sym[_]) = defUseMap.getOrElse(sym, 0)

  def buildDefUse(body: Block[Any]): Unit = {
    defUseMap.clear()
    
    refSyms(getBlockResult(body)).foreach(addDefUse)
    for (TP(_, rhs) <- buildScheduleForResult(body, false /*unsorted*/)) {
      for (sym <- refSyms(rhs)) {
	addDefUse(sym)
      }
    }
  }

  def refSyms(e: Any): List[Sym[Any]] = e match {
    case Reify(x, u, es) => readSyms(x)
    case _ => readSyms(e)
  }

  abstract override def emitSourceAnyArity(args: List[Exp[Any]], body: Block[Any], methName: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    buildDefUse(body)
    super.emitSourceAnyArity(args, body, methName, stream)
  }

  abstract override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (defUse(sym) == 0) stream.println(rhs)
    else super.emitValDef(sym, rhs)
  }
}

trait JSCodegenOpt extends JSNestedCodegen with NestedCodegenOpt

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
