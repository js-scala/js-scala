import scala.virtualization.lms.common._
import scala.virtualization.lms.util.ClosureCompare

import java.io.PrintWriter

trait JSFunctions extends Functions {
  implicit def fun[A:Manifest,B:Manifest](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)
}

trait JSFunctionsExp extends JSFunctions with FunctionsRecursiveExp

trait JSGenFunctions extends JSGenEffect {
  val IR: JSFunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println("var " + quote(sym) + " = function(" + quote(x) + ") { ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")

    case e@Lambda2(fun, x1, x2, y) =>
      stream.println("var " + quote(sym) + " = function(" + quote(x1) + ", " + quote(x2) + ") { ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}
