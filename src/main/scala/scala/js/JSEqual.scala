package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter

trait JSGenEqual extends JSGenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case NotEqual(a,b) =>  emitValDef(sym, "" + quote(a) + "!=" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
