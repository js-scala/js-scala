package scala.js

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import java.io.PrintWriter

trait JSCodegen extends GenericCodegen {
  import IR._
  
  def emitHTMLPage[B](f: () => Exp[B], stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    stream.println("<html><head><title>Scala2JS</title><script type=\"text/JavaScript\">")

    emitSource((x:Exp[Int]) => f(), "main", stream)

    stream.println("</script><body onload=\"main(0)\">")
    stream.println("</body></html>")
    stream.flush
  }

  def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    val y = f()
    emitSourceAnyArity(Nil, y, methName, stream)
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    val x = fresh[A]
    val y = f(x)

    emitSourceAnyArity(List(x), y, methName, stream)
  }

  def emitSource2[A1,A2,B](f: (Exp[A1], Exp[A2]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): Unit = {
    val x1 = fresh[A1]
    val x2 = fresh[A2]
    val y = f(x1, x2)

    emitSourceAnyArity(List(x1, x1), y, methName, stream)
  }

  def emitSource5[A1,A2,A3,A4,A5,B](f: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mB: Manifest[B]): Unit = {
    val x1 = fresh[A1]
    val x2 = fresh[A2]
    val x3 = fresh[A3]
    val x4 = fresh[A4]
    val x5 = fresh[A5]

    val y = f(x1, x2, x3, x4, x5)

    emitSourceAnyArity(List(x1, x2, x3, x4, x5), y, methName, stream)
  }

  private def emitSourceAnyArity(args: List[Exp[Any]], body: Exp[Any], methName: String, stream: PrintWriter): Unit = {
    val argsStr = args.map(quote).mkString(", ")
    stream.println("function "+methName+"("+argsStr+") {")

    emitBlock(body)(stream)
    stream.println("return "+quote(getBlockResult(body)))

    stream.println("}")
    stream.flush
  }

  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }

  def emitVarDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
    emitValDef(sym, rhs)
  }

  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case Const(()) => "undefined"
    case _ => super.quote(x)
  }
}

trait JSNestedCodegen extends GenericNestedCodegen with JSCodegen {
  import IR._

  override def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    super.emitSource0(() => reifyEffects(f()), methName, stream)
  }

  // TODO: we shouldn't need the manifests here (aks)
  override def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitSource[A,B](x => reifyEffects(f(x)), methName, stream)
  }

  override def emitSource2[A1,A2,B](f: (Exp[A1], Exp[A2]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): Unit = {
      super.emitSource2[A1,A2,B]((x1, x2) => reifyEffects(f(x1, x2)), methName, stream)
  }

  override def emitSource5[A1,A2,A3,A4,A5,B](f: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mB: Manifest[B]): Unit = {
      super.emitSource5[A1,A2,A3,A4,A5,B]((x1, x2, x3, x4, x5) => reifyEffects(f(x1, x2, x3, x4, x5)), methName, stream)
  }

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => sys.error("Sym(-1) not supported")
    case _ => super.quote(x)
  }
}

trait JSGenBase extends JSCodegen {
  val IR: BaseExp
}

trait JSGenEffect extends JSNestedCodegen with JSGenBase {
  val IR: EffectExp  
}


trait JSGenIfThenElse extends BaseGenIfThenElse with JSGenEffect { // it's more or less generic...
  val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case IfThenElse(c,a,b) =>  
      stream.println("var " + quote(sym))
      stream.println("if (" + quote(c) + ") {")
      emitBlock(a)
      stream.println(quote(sym) + "=" + quote(getBlockResult(a)))
      stream.println("} else {")
      emitBlock(b)
      stream.println(quote(sym) + "=" + quote(getBlockResult(b)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenNumericOps extends JSGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
