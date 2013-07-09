package scala.js.gen.js

import scala.js.exp.ArraysExp

trait GenArrays extends GenEffect {
  val IR: ArraysExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayLiteral(xs) => emitValDef(sym, xs.map(quote).mkString("[", ", ", "]"))
    case ArrayApply(a, i) => emitValDef(sym, quote(a) + "[" + quote(i) + "]")
    case ArrayLength(a) => emitValDef(sym, quote(a) + ".length")
    case ArrayUpdate(a, i, x) => emitValDef(sym, quote(a) + "[" + quote(i) + "] = " + quote(x))
    case ArrayForeach(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".forEach(")
      stream.println("function(" + quote(x) + ",i_,a_){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case ArrayMap(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case ArrayFlatMap(a, x, block) =>
      stream.println("var " + quote(sym) + " = []")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0; " + quote(i) + "< " + quote(a) + ".length; " + quote(i) + "++){")
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length, 0].concat((function(" + quote(x) + "){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})(" + quote(a) + "[" + quote(i) + "])))")
      stream.println("}")
    case ArrayFilter(a, x, block) =>
      stream.println("var " + quote(sym) + "=" + quote(a) + ".filter(")
      stream.println("function(" + quote(x) + ",i_,a_){")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("})")
    case ArrayJoin(a, s) => emitValDef(sym, quote(a) + ".join(" + quote(s) + ")")
    case ArrayToList(a) => emitValDef(sym, quote(a) + ".splice(0)")
    case RangeForeach(Range(a, b), i, block) =>
      emitValDef(sym, "undefined")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println("}")
    case RangeMap(Range(a, b), i, block) =>
      stream.println("var " + quote(sym) + " = []")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println(quote(sym) + "[" + quote(i) + "]=" + quote(getBlockResult(block)))
      stream.println("}")
    case RangeFlatMap(Range(a, b), i, block) =>
      stream.println("var " + quote(sym) + " = []")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length,0].concat(" + quote(getBlockResult(block)) + "))")
      stream.println("}")
    case RangeFilter(Range(a, b), i, block) =>
      stream.println("var " + quote(sym) + " = []")
      stream.println("for(var " + quote(i) + "=" + quote(a) + ";" + quote(i) + "<" + quote(b) + ";" + quote(i) + "++){")
      emitBlock(block)
      stream.println("if (" + quote(getBlockResult(block)) + ") {")
      stream.println(quote(sym) + "[" + quote(sym) + ".length]=" + quote(getBlockResult(block)))
      stream.println("}}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait GenArraysLegacy extends GenArrays {
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayForeach(a, x, block) =>
      val i = fresh[Int]
      val l = fresh[Int]
      val f = fresh[Unit]
      stream.println("var " + quote(f) + " = function ( " + quote(x) + ") {")
      emitBlock(block)
      stream.println("};")
      stream.println("for (var " + quote(i) + " = 0, " + quote(l) + " = " + quote(a) + ".length ; " + quote(i) + " < " + quote(l) + " ; " + quote(i) + "++) {")
      stream.println(quote(f) + "(" + quote(a) + "[" + quote(i) + "]);")
      stream.println("}")
      stream.println("var " + quote(sym) + ";")
    case ArrayMap(a, x, block) =>
      val i = fresh[Int]
      val l = fresh[Int]
      val f = fresh[Unit]
      emitValDef(sym, "[]")
      stream.println("var " + quote(f) + " = function (" + quote(x) + ") {")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("};")
      stream.println("for (var " + quote(i) + " = 0, " + quote(l) + " = " + quote(a) + ".length ; " + quote(i) + " < " + quote(l) + " ; " + quote(i) + "++) {")
      stream.println(quote(sym) + "[" + quote(i) + "] = " + quote(f) + "(" + quote(a) + "[" + quote(i) + "]);")
      stream.println("}")
    case ArrayFilter(a, x, block) =>
      val i = fresh[Int]
      val l = fresh[Int]
      val p = fresh[Unit]
      emitValDef(sym, "[]")
      stream.println("var " + quote(p) + " = function (" + quote(x) + ") {")
      emitBlock(block)
      stream.println("return " + quote(getBlockResult(block)))
      stream.println("};")
      stream.println("for (var " + quote(i) + " = 0, " + quote(l) + " = " + quote(a) + ".length ; " + quote(i) + " < " + quote(l) + " ; " + quote(i) + "++) {")
      stream.println("if (" + quote(p) + "(" + quote(a) + "[" + quote(i) + "])) {")
      stream.println(quote(sym) + ".push(" + quote(a) + "[" + quote(i) + "]);")
      stream.println("}")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}