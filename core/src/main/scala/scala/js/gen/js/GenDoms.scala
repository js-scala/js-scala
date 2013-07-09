package scala.js.gen.js

import scala.js.exp.DomsExp

trait GenDoms extends GenProxy {
  val IR: DomsExp
  import IR._
  override def quote(x: Exp[Any]) : String = x match {
    case DocumentVar => "document"
    case _ => super.quote(x)
  }
}