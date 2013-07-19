package scala.js.gen.js

import scala.js.exp.FFIExp

trait GenFFI extends GenBase {
  val IR: FFIExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ForeignExpression(ctx, args) =>
      ctx.checkLengths(args)
      val pi = ctx.parts.iterator
      val ai = args.iterator
      val bldr = new StringBuilder(StringContext.treatEscapes(pi.next()))
      while (ai.hasNext) {
        bldr.append(quote(ai.next()))
        bldr.append(StringContext.treatEscapes(pi.next()))
      }
      emitValDef(sym, bldr.toString())
    case _ => super.emitNode(sym, rhs)
  }

}
