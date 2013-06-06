package scala.js.gen.js

import scala.virtualization.lms.common.TupleOpsExp

trait GenTupleOps extends GenBase {
  val IR: TupleOpsExp
  import IR._

  // flag that manages how tuples are translated into JS
  // by default they are translated to JS objects with fields
  // named '_1', '_2' and so on
  // for interoperatbility one can override this setting so that
  // tuples are translated to arrays
  def tupleAsArrays = false

  def access(idx: Int): String =
    if(tupleAsArrays)
      s"[${idx-1}]"
    else
      s"._$idx"

  def build(elems: Exp[_]*): String =
    if(tupleAsArrays) {
      "[" + elems.map(quote _).mkString(",") + "]"
    } else {
      val withIdx = elems.zipWithIndex.map {
        case (elem, idx) => "_" + (idx + 1) + ":" + quote(elem)
      }
      "{" + withIdx.mkString(",") + "}"
    }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ETuple2(a,b)        => emitValDef(sym, build(a, b))
    case Tuple2Access1(t)    => emitValDef(sym, quote(t) + access(1))
    case Tuple2Access2(t)    => emitValDef(sym, quote(t) + access(2))

    case ETuple3(a,b,c)      => emitValDef(sym, build(a, b, c))
    case Tuple3Access1(t)    => emitValDef(sym, quote(t) + access(1))
    case Tuple3Access2(t)    => emitValDef(sym, quote(t) + access(2))
    case Tuple3Access3(t)    => emitValDef(sym, quote(t) + access(3))

    case ETuple4(a,b,c,d)    => emitValDef(sym, build(a, b, c, d))
    case Tuple4Access1(t)    => emitValDef(sym, quote(t) + access(1))
    case Tuple4Access2(t)    => emitValDef(sym, quote(t) + access(2))
    case Tuple4Access3(t)    => emitValDef(sym, quote(t) + access(3))
    case Tuple4Access4(t)    => emitValDef(sym, quote(t) + access(4))

    case ETuple5(a,b,c,d,e)  => emitValDef(sym, build(a, b, c, d, e))
    case Tuple5Access1(t)    => emitValDef(sym, quote(t) + access(1))
    case Tuple5Access2(t)    => emitValDef(sym, quote(t) + access(2))
    case Tuple5Access3(t)    => emitValDef(sym, quote(t) + access(3))
    case Tuple5Access4(t)    => emitValDef(sym, quote(t) + access(4))
    case Tuple5Access5(t)    => emitValDef(sym, quote(t) + access(5))

    case _ => super.emitNode(sym, rhs)
  }

}
