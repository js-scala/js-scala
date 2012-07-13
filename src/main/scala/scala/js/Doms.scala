package scala.js

import scala.virtualization.lms.common._

trait Doms extends JSProxyBase {
  trait Element {
    def getElementById(id: Rep[String]): Rep[Element]
  }
  trait Canvas extends Element {
    def getContext(context: Rep[String]): Rep[Context]
  }
  trait Context {
    def save(): Rep[Unit]
    def lineTo(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def scale(x1: Rep[Double], x2: Rep[Double]): Rep[Unit]
    def rotate(x: Rep[Double]): Rep[Unit]
    def restore(): Rep[Unit]
    def translate(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def moveTo(x: Rep[Int], y: Rep[Int])
    def closePath(): Rep[Unit]
    def stroke(): Rep[Unit]
    var fillStyle: Rep[String]
    def fillRect(x: Rep[Int], y: Rep[Int], width: Rep[Int], height: Rep[Int])
  }
  val document: Rep[Element]
  implicit def repToElement(x: Rep[Element]): Element = repProxy[Element](x)
  implicit def repToCanvas(x: Rep[Canvas]): Canvas = repProxy[Canvas](x)
  implicit def repToContext(x: Rep[Context]): Context = repProxy[Context](x)
}

trait DomsExp extends Doms with JSProxyExp {
  case object DocumentVar extends Exp[Element]
  val document = DocumentVar
}

trait GenDoms extends JSGenProxy {
  val IR: DomsExp
  import IR._
  override def quote(x: Exp[Any]) : String = x match {
    case DocumentVar => "document"
    case _ => super.quote(x)
  }
}
