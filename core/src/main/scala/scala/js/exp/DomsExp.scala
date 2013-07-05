package scala.js.exp

import scala.js.language.Doms

trait DomsExp extends Doms with ProxyExp {
  case object DocumentVar extends Exp[Element]
  val document = DocumentVar
}