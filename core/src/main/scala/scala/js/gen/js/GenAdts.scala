package scala.js.gen.js

import scala.js.exp.AdtsExp

trait GenAdts extends GenBase with GenFunctions {
  val IR: AdtsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case AdtApply(fields) => emitValDef(sym, literalObjectDef(fields))
    case AdtSelect(obj, label) => emitValDef(sym, literalObjectSelect(obj,label))
    case AdtEqual(obj, bis, fields) =>
      emitValDef(sym, fields.map(quote).mkString(" && "))
    case AdtFieldEqual(a1, a2, field) =>
      emitValDef(sym, s"${quote(a1)}.$field === ${quote(a2)}.$field")
    case AdtFold(obj, fs) =>
      emitValDef(sym, "["+fs.map(quote).mkString(",")+"]["+quote(obj)+".$variant]("+quote(obj)+")")
    case _ => 
      super.emitNode(sym, rhs)
       
  }
}