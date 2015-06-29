package scala.js.gen.js

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericCodegen}
import scala.reflect.NameTransformer
import java.io.PrintWriter
import scala.js.gen.QuoteGen

trait Codegen extends GenericCodegen {
  import IR._

  def emitHTMLPage[B](f: () => Exp[B], stream: PrintWriter)(implicit mB: Manifest[B]): Unit = {
    stream.println("<html><head><title>Scala2JS</title><script type=\"text/JavaScript\">")

    emitSource((x:Exp[Int]) => f(), "main", stream)

    stream.println("</script><body onload=\"main(0)\">")
    stream.println("</body></html>")
    stream.flush
  }

  def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val y = reifyBlock(f())
    emitSource(Nil, y, methName, stream)
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], name: String, out: PrintWriter) = {
    val argsStr = args.map(quote).mkString(", ")

    withStream(out) {
      stream.println("function "+(if (name.isEmpty) "" else (name))+"("+argsStr+") {")
      emitBlock(body)
      val result = getBlockResult(body)
      if (!(result.tp <:< manifest[Unit])) {
        stream.println("return "+quote(result))
      }

      stream.println("}")
      stream.flush()
    }
    getFreeDataBlock(body)
  }

  def emitExecution[A : Manifest](a: => Exp[A], out: PrintWriter): List[(Sym[Any], Any)] = {
    val b = reifyBlock(a)
    out.print("(")
    emitSource(Nil, b, "", out)
    out.println(")()")
    getFreeDataBlock(b)
  }

  override def emitValDef(sym: Sym[Any], rhs: String) {
    stream.println("var " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Any], rhs: String) {
    emitValDef(sym, rhs)
  }

  def emitAssignment(lhs: String, rhs: String) {
    stream.println(lhs + " = " + rhs + ";")
  }

  override def quote(x: Exp[Any]) : String = x match {
    case Const(()) => "undefined"
    case null => "null" // why?
    case _ => super.quote(x)
  }

  def literalObjectDef(fields: Seq[(String, Exp[_])]): String = {
    fields.map({case (name, value) => "'" + NameTransformer.decode(name) + "' : " + quote(value)}).mkString("{", ",", "}")
  }

  def literalObjectSelect(receiver: Exp[_], field: String): String = {
    val decodedField = NameTransformer.decode(field)
    if (decodedField == field) quote(receiver) + "." + field
    else quote(receiver) + "['" + decodedField + "']"
  }

}

trait NestedCodegen extends GenericNestedCodegen with Codegen {
  import IR._

  override def quote(x: Exp[Any]) = x match { // TODO: quirk!
    case Sym(-1) => sys.error("Sym(-1) not supported")
    case _ => super.quote(x)
  }
}

trait GenBase extends Codegen {
  val IR: BaseExp
}

trait GenEffect extends NestedCodegen with GenBase {
  val IR: EffectExp
}


trait GenIfThenElse extends BaseGenIfThenElse with GenEffect { // it's more or less generic...
  val IR: IfThenElseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

trait GenNumericOps extends GenPrimitiveOps {
  val IR: NumericOpsExp with PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus(a,b) => emitValDef(sym, quote(a) + " + " + quote(b))
    case NumericMinus(a,b) => emitValDef(sym, quote(a) + " - " + quote(b))
    case NumericTimes(a,b) => emitValDef(sym, quote(a) + " * " + quote(b))
    case NumericDivide(a,b) => emitValDef(sym, quote(a) + " / " + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}

trait GenOrderingOps extends GenBase {
  val IR: OrderingOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

trait GenWhile extends GenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

trait GenBooleanOps extends GenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait GenStringOps extends GenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep, Const(0)) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringValueOf(a) => emitValDef(sym, "String(%s)".format(quote(a)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait GenObjectOps extends GenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(o) => emitValDef(sym, "(" + quote(o) + ").toString()")
    case ObjectUnsafeImmutable(o) => emitValDef(sym, quote(o))
    case ObjectUnsafeMutable(o) => emitValDef(sym, quote(o))
    case _ => super.emitNode(sym, rhs)
  }
}


trait GenListOps extends GenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListNew(values) => {
      emitValDef(sym, values.map(quote).mkString("[", ",", "]"))
    }
    case ListFlatMap(l, x, b) => {
      stream.println("var " + quote(sym) + " = [];")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0 ; " + quote(i) + " < " + quote(l) + ".length ; " + quote(i) + "++){")
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length, 0].concat((function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("})(" + quote(l) + "[" + quote(i) + "])));")
      stream.println("}")
    }
    case ListMap(l, x, b) => {
      stream.print("var " + quote(sym) + "=" + quote(l) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    }
    case ListFilter(l, x, b) =>
      stream.print("var " + quote(sym) + "=" + quote(l) + ".filter(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    case ListPrepend(l, e) => emitValDef(sym, "[" + quote(e) + "].concat(" + quote(l) + ")")
    case ListConcat(l1, l2) => emitValDef(sym, quote(l1) + ".concat(" + quote(l2) + ")")
    case ListMkString(l) => emitValDef(sym, quote(l) + ".join('')")
    case ListMkString2(l, sep) => emitValDef(sym, quote(l) + ".join(" + quote(sep) + ")")
    case ListHead(l) => emitValDef(sym, quote(l) + "[0]")
    case ListTail(l) => emitValDef(sym, quote(l) + ".slice(1)")
    case ListIsEmpty(l) => emitValDef(sym, quote(l) + ".length == 0")
    case _ => super.emitNode(sym, rhs)
  }
}



trait GenPrimitiveOps extends GenBase with QuoteGen {
  val IR: PrimitiveOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, q"parseFloat($s)")
    case ObjDoublePositiveInfinity() => emitValDef(sym, "Infinity")
    case ObjDoubleNegativeInfinity() => emitValDef(sym, "-Infinity")
    case ObjDoubleMinValue() => emitValDef(sym, "Number.MIN_VALUE")
    case ObjDoubleMaxValue() => emitValDef(sym, "Number.MAX_VALUE")
    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs))
    case ObjIntegerParseInt(s) => emitValDef(sym, q"parseInt($s, 10)")
    case ObjIntMaxValue() => emitValDef(sym, "Number.MAX_VALUE")
    case ObjIntMinValue() => emitValDef(sym, "Number.MIN_VALUE")
//    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, "Math.floor(" + quote(lhs) + " / " + quote(rhs) + ")")
    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs))
    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs))
    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
    case IntToLong(lhs) => emitValDef(sym, quote(lhs))
    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case LongToInt(lhs) => emitValDef(sym, quote(lhs))
    case IntPlus(lhs, rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case IntTimes(lhs, rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
    case IntMinus(lhs, rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
}


trait GenMiscOps extends GenBase {
  val IR: MiscOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case PrintLn(s) => emitValDef(sym, "console.log(" + quote(s) + ")")
    case Print(s) => emitValDef(sym, "console.log(" + quote(s) + ")")
    case Error(s) => stream.println("throw " + quote(s) + ";")
    case _ => super.emitNode(sym, rhs)
  }
}
