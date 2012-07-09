package scala.js

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import java.io.PrintWriter

trait Codegen extends GenericCodegen {
  import IR._

  def emitSourceAnyArity(args: List[Exp[Any]], body: Exp[Any], methName: String, stream: PrintWriter): List[(Sym[Any], Any)]
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
}

trait JSCodegen extends Codegen {
  import IR._

  def emitHTMLPage[B](f: () => Exp[B], stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    stream.println("<html><head><title>Scala2JS</title><script type=\"text/JavaScript\">")

    emitSource((x:Exp[Int]) => f(), "main", stream)

    stream.println("</script><body onload=\"main(0)\">")
    stream.println("</body></html>")
    stream.flush
  }

  def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val y = f()
    emitSourceAnyArity(Nil, y, methName, stream)
  }

  def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val x = fresh[A]
    val y = f(x)

    emitSourceAnyArity(List(x), y, methName, stream)
  }

  override def emitSourceAnyArity(args: List[Exp[Any]], body: Exp[Any], methName: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val argsStr = args.map(quote).mkString(", ")
    stream.println("function"+(if (methName.isEmpty) "" else (" "+methName))+"("+argsStr+") {")

    emitBlock(body)(stream)
    stream.println("return "+quote(getBlockResult(body)))

    stream.println("}")
    stream.flush
    getFreeDataBlock(body)
  }

  override def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {
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

  override def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): List[(Sym[Any], Any)] = {
    super.emitSource0(() => reifyEffects(f()), methName, stream)
  }

  // TODO: we shouldn't need the manifests here (aks)
  override def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    super.emitSource[A,B](x => reifyEffects(f(x)), methName, stream)
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

trait JSTupledCodegen extends JSCodegen {
  val IR: TupleOpsExp
  import IR._

  case class UnboxedTuple[T: Manifest](val vars: List[Exp[Any]]) extends Exp[T]

  override def emitSource[A,B](f: Exp[A] => Exp[B], methName: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    def tupledManifest[T](m: Manifest[T]): Boolean = m.erasure.getName startsWith "scala.Tuple"
      val (args, x) = if (tupledManifest(mA)) {
      val args = mA.typeArguments.map(fresh(_))
      (args, UnboxedTuple[A](args))
    } else {
      val x = fresh[A]
      (List(x), x)
    }
    val y = f(x)
    emitSourceAnyArity(args, y, methName, stream)
  }

  def emitSource[A1,A2,B](f: (Exp[A1], Exp[A2]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): Unit = {
    val f1 = (t: Exp[(A1,A2)]) => f(tuple2_get1(t), tuple2_get2(t))
    emitSource(f1, methName, stream)
  }

  def emitSource[A1,A2,A3,A4,A5,B](f: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5]) => Exp[B], methName: String, stream: PrintWriter)(
    implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mB: Manifest[B]): Unit = {
    val f1 = (t: Exp[(A1,A2,A3,A4,A5)]) => f(tuple5_get1(t), tuple5_get2(t), tuple5_get3(t), tuple5_get4(t), tuple5_get5(t))

    super.emitSource(f1, methName, stream)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Tuple2Access1(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(0)))
    case Tuple2Access2(UnboxedTuple(vars)) => emitValDef(sym, quote(vars(1)))
    case _ => super.emitNode(sym, rhs)
  }
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

trait JSGenOrderingOps extends JSGenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case OrderingLT(a,b) => emitValDef(sym, quote(a) + " < " + quote(b))
    case OrderingLTEQ(a,b) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case OrderingGT(a,b) => emitValDef(sym, quote(a) + " > " + quote(b))
    case OrderingGTEQ(a,b) => emitValDef(sym, quote(a) + " >= " + quote(b))
    case OrderingEquiv(a,b) => emitValDef(sym, quote(a) + " == " + quote(b))
    case OrderingMax(a,b) => emitValDef(sym, "Math.max(" + quote(a) + ", " + quote(b) + ")")
    case OrderingMin(a,b) => emitValDef(sym, "Math.min(" + quote(a) + ", " + quote(b) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenWhile extends JSGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case While(c,b) =>
      emitValDef(sym, "undefined")
      val cond_fun = "cond_" + quote(sym)
      stream.println("function " + cond_fun + "() {")
      emitBlock(c)
      stream.println("return " + quote(getBlockResult(c)))
      stream.println("}")
      stream.println("while (" + cond_fun + "()) {")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenBooleanOps extends JSGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait JSGenStringOps extends JSGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringValueOf(a) => emitValDef(sym, "String(%s)".format(quote(a)))
    case _ => super.emitNode(sym, rhs)
  }
}

