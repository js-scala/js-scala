package scala.js

import scala.virtualization.lms.common._

trait JSGenListOps extends JSGenEffect {
  val IR: ListOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ListNew(values) => {
      emitValDef(sym, values.map(quote).mkString("[", ",", "]"))
    }
    case ListFlatMap(l, x, b) => {
      stream.println("var " + quote(sym) + " = [];")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0 ; " + quote(i) + " < " + quote(l) + ".length ; " + quote(i) + "++){")
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length, 0].concat((function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("})(" + quote(l) + "[" + quote(i) + "])));")
      stream.println("}")
    }
    case ListMap(l, x, b) => {
      stream.print("var " + quote(sym) + "=" + quote(l) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    }
    case ListPrepend(l, e) => emitValDef(sym, "[" + quote(e) + "].concat(" + quote(l) + ")")
    case ListConcat(l1, l2) => emitValDef(sym, quote(l1) + ".concat(" + quote(l2) + ")")
    case ListMkString(l) => emitValDef(sym, quote(l) + ".join('')")
    case ListHead(l) => emitValDef(sym, quote(l) + "[0]")
    case ListTail(l) => emitValDef(sym, quote(l) + ".slice(1)")
    case ListIsEmpty(l) => emitValDef(sym, quote(l) + ".length == 0")
    case _ => super.emitNode(sym, rhs)
  }
}
