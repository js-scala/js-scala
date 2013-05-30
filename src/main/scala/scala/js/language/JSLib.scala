package scala.js.language

import scala.virtualization.lms.common.Structs

trait JSLib extends Proxy with Structs {
  val window: Rep[Window]
  trait Window {
    def setTimeout(func: Rep[Unit => Unit], delay: Rep[Int]): Rep[Int]
  }
  implicit def repToWindow(x: Rep[Window]): Window = repProxy[Window](x)

  val json: Rep[JSON]
  trait JSON {
    def stringify(literal: Rep[Record]): Rep[String]
    def parse[T <: Record](data: Rep[String]): Rep[T]
  }
  implicit def repToJSON(x: Rep[JSON]): JSON = repProxy[JSON](x)

}