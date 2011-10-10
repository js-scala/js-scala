package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait JSGenVariables extends JSGenEffect {
  val IR: VariablesExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ReadVar(Variable(a)) => emitValDef(sym, quote(a))
    case NewVar(init) => emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], quote(getBlockResult(init)))
    case Assign(Variable(a), b) => emitAssignment(quote(a), quote(getBlockResult(b)))
    case VarPlusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " += " + quote(getBlockResult(b)))
    case VarMinusEquals(Variable(a), b) => emitValDef(sym, quote(a) + " -= " + quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}
