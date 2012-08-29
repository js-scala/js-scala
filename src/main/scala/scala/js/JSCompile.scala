package scala.js

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.NameTransformer

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

  def emitSource0[B](f: () => Exp[B], methName: String, stream: PrintWriter)(implicit mB: Manifest[B]): List[(Sym[Any], Any)] = {
    val y = reifyBlock(f())
    emitSource(Nil, y, methName, stream)
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], name: String, out: PrintWriter) = {
    val argsStr = args.map(quote).mkString(", ")

    withStream(out) {
      stream.println("function"+(if (name.isEmpty) "" else (" "+name))+"("+argsStr+") {")
      emitBlock(body)
      stream.println("return "+quote(getBlockResult(body)))

      stream.println("}")
      stream.flush
    }
    getFreeDataBlock(body)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Any], rhs: String): Unit = {
    emitValDef(sym, rhs)
  }

  def emitAssignment(lhs: String, rhs: String): Unit = {
    stream.println(lhs + " = " + rhs)
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

trait JSNestedCodegen extends GenericNestedCodegen with JSCodegen {
  import IR._

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

trait JSGenNumericOps extends JSGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
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

trait JSGenWhile extends JSGenEffect with BaseGenWhile {
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

trait JSGenBooleanOps extends JSGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym,rhs)
  }
}

trait JSGenStringOps extends JSGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringValueOf(a) => emitValDef(sym, "String(%s)".format(quote(a)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenObjectOps extends JSGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(o) => emitValDef(sym, "(" + quote(o) + ").toString()")
    case ObjectUnsafeImmutable(o) => emitValDef(sym, quote(o))
    case ObjectUnsafeMutable(o) => emitValDef(sym, quote(o))
    case _ => super.emitNode(sym, rhs)
  }
}


trait JSGenListOps extends JSGenEffect {
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
    case ListPrepend(l, e) => emitValDef(sym, "[" + quote(e) + "].concat(" + quote(l) + ")")
    case ListConcat(l1, l2) => emitValDef(sym, quote(l1) + ".concat(" + quote(l2) + ")")
    case ListMkString(l) => emitValDef(sym, quote(l) + ".join('')")
    case ListHead(l) => emitValDef(sym, quote(l) + "[0]")
    case ListTail(l) => emitValDef(sym, quote(l) + ".slice(1)")
    case ListIsEmpty(l) => emitValDef(sym, quote(l) + ".length == 0")
    case _ => super.emitNode(sym, rhs)
  }
}

