package scala.js

import scala.virtualization.lms.common._

trait JSGenStruct extends JSGenBase {

  val IR: StructExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(_, elems) =>
      emitValDef(sym, literalObjectDef(elems.toSeq))
    case Field(struct, index, _) =>
      emitValDef(sym, literalObjectSelect(struct, index))
    case _ => super.emitNode(sym, rhs)
  }

}