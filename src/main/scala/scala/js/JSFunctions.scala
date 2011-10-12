package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait JSFunctions extends Functions {
}

trait JSFunctionsExp extends JSFunctions with FunctionsRecursiveExp

trait JSGenFunctions extends JSGenEffect with BaseGenFunctions {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("var " + quote(sym) + " = function(" + quote(x) + ") {")
      emitBlock(y)
      stream.println("return " + quote(getBlockResult(y)))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
