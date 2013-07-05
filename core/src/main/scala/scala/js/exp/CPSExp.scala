package scala.js.exp

import scala.js.language.CPS

trait CPSExp extends CPS with ProxyExp {
  
  case class CellNode[A: Manifest]() extends Def[Cell[A]]
  
  def createCell[A: Manifest](): Rep[Cell[A]] = reflectEffect(CellNode[A]())

}