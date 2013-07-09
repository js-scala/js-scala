package scala.js.gen.js

import scala.js.exp.RecordsExp

trait GenRecords extends GenBase {
  val IR: RecordsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RecordApply(fields) => emitValDef(sym, literalObjectDef(fields))
    case RecordSelect(obj,label) => emitValDef(sym, literalObjectSelect(obj,label))
    case _ => super.emitNode(sym, rhs)
  }
}