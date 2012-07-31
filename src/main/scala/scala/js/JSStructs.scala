package scala.js

import scala.virtualization.lms.common._

trait JSGenStruct extends JSGenBase { this: JSGenLiteral with JSGenStruct =>

  val IR: StructExp with JSLiteralExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Struct(_, elems) =>
      emitNode(sym, JSLiteralDef(elems.toList))
    case Field(struct, index, _) =>
      emitNode(sym, MemberSelect(struct, index))
    case _ => super.emitNode(sym, rhs)
  }

}