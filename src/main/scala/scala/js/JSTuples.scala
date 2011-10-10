package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait JSGenTupleOps extends JSGenBase {
  val IR: TupleOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ETuple2(a,b)  =>
      emitValDef(sym, "{_1:"+ quote(a) + ",_2:" + quote(b) + "}")
    case Tuple2Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple2Access2(t) => emitValDef(sym, quote(t) + "._2")

    case ETuple3(a,b,c)  =>
      emitValDef(sym, "{_1:"+ quote(a) + ",_2:" + quote(b) + ",_3:" + quote(c) + "}")
    case Tuple3Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple3Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple3Access3(t) => emitValDef(sym, quote(t) + "._3")

    case ETuple4(a,b,c,d)  =>
      emitValDef(sym, "{_1:"+ quote(a) + ",_2:" + quote(b) + ",_3:" + quote(c) + ",_4:" + quote(d) + "}")
    case Tuple4Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple4Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple4Access3(t) => emitValDef(sym, quote(t) + "._3")
    case Tuple4Access4(t) => emitValDef(sym, quote(t) + "._4")

    case ETuple5(a,b,c,d,e)  =>
      emitValDef(sym, "{_1:"+ quote(a) + ",_2:" + quote(b) + ",_3:" + quote(c) + ",_4:" + quote(d) + ",_5:" + quote(e) + "}")
    case Tuple5Access1(t) => emitValDef(sym, quote(t) + "._1")
    case Tuple5Access2(t) => emitValDef(sym, quote(t) + "._2")
    case Tuple5Access3(t) => emitValDef(sym, quote(t) + "._3")
    case Tuple5Access4(t) => emitValDef(sym, quote(t) + "._4")
    case Tuple5Access5(t) => emitValDef(sym, quote(t) + "._5")

    case _ => super.emitNode(sym, rhs)
  }

}
