package scala.js.gen.js

import scala.js.exp.JSMapsExp
import scala.js.gen.QuoteGen

/** Generates javascript code that deals with maps.
 *  Higher order functions `mapValues`, `filter` and `foreach` are transformed
 *  to loops and function definitions.
 *
 *  @author Lucas Satabin
 */
trait GenJSMaps extends GenEffect with QuoteGen {
  val IR: JSMapsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MapNew()               => emitValDef(sym, "{}")
    case MapApply(m,k)          => emitValDef(sym, q"$m[$k]")
    case MapGet(m,k)            => emitValDef(sym, q"$m[$k]")
    case MapUpdate(m,k,v)       => emitValDef(sym, q"$m[$k] = $v")
    case MapContains(m,i)       => emitValDef(sym, q"$i in $m")
    case MapSize(m)             => emitValDef(sym, q"Object.keys($m).length")
    case MapValues(m)           =>
      emitValDef(sym, q"Object.keys($m).map(function(k) { return $m[k] })")
    case MapClear(m)            =>
      stream.println(q"var $sym = Object.keys($m).forEach(function(_el) {")
      stream.println(q"delete $m[_el];")
      stream.println("});")
    case MapKeys(m)             => emitValDef(sym, q"Object.keys($m)")
    case MapForeach(m, k, v, b) =>
      stream.println(q"var $sym = Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      stream.println("});")
    case MapMapValues(m, v, b)  =>
      val k = fresh[Any]
      stream.println(q"var $sym = {};")
      stream.println(q"Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      val res = getBlockResult(b)
      stream.println(q"$sym[$k] = $res;")
      stream.println("});")
    case MapFilter(m, k, v, b)  =>
      stream.println(q"var $sym = {};")
      stream.println(q"Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      val res = getBlockResult(b)
      stream.println(q"if($res) {")
      stream.println(q"$sym[$k] = $v")
      stream.println("}")
      stream.println("});")
    case _                      => super.emitNode(sym, rhs)
  }
}