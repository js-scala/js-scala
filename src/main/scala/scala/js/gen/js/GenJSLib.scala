package scala.js.gen.js

import scala.js.exp.JSLibExp

trait GenJSLib extends GenProxy with GenStruct {
  val IR: JSLibExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case WindowVar => "window"
    case JSONVar => "JSON"
    case _ => super.quote(x)
  }
}