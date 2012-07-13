package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait JSFunctions extends TupledFunctions

trait JSFunctionsExp extends JSFunctions with TupledFunctionsRecursiveExp

trait JSGenFunctions extends JSGenEffect with BaseGenFunctions {
  val IR: TupledFunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      stream.println("var " + quote(sym) + " = function" + xs.map(quote).mkString("(", ",", ")") + " {")
      emitBlock(y)
      stream.println("return " + quote(getBlockResult(y)))
      stream.println("}")

    case Lambda(fun, x, y) =>
      stream.println("var " + quote(sym) + " = function(" + quote(x) + ") {")
      emitBlock(y)
      stream.println("return " + quote(getBlockResult(y)))
      stream.println("}")

    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case UnboxedTuple(args) =>
      args.zipWithIndex.map( { case (v,i) => "_" + (i+1) + ":" + quote(v) } ).mkString("{", ",", "}")
    case _ => super.quote(x)
  }
}
