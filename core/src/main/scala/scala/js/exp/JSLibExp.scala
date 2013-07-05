package scala.js.exp

import scala.js.language.JSLib
import scala.virtualization.lms.common.StructExp

trait JSLibExp extends JSLib with ProxyExp with StructExp {
  case object WindowVar extends Exp[Window]
  val window = WindowVar

  case object JSONVar extends Exp[JSON]
  val json = JSONVar
}