package scala.js

trait Doms extends JSProxyBase {
  trait Element
  trait ElementOps {
    def getElementById(id: Rep[String]): Rep[Element]
  }
  trait Canvas
  trait CanvasOps {
    def getContext(context: Rep[String]): Rep[Context]
  }
  trait Context extends Element
  trait ContextOps extends ElementOps {
    def save(): Rep[Unit]
    def lineTo(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def scale(x1: Rep[Double], x2: Rep[Double]): Rep[Unit]
    def rotate(x: Rep[Double]): Rep[Unit]
    def restore(): Rep[Unit]
    def translate(x: Rep[Int], y: Rep[Int]): Rep[Unit]
    def moveTo(x: Rep[Int], y: Rep[Int])
    def closePath(): Rep[Unit]
    def stroke(): Rep[Unit]
  }
  trait AsOps {
    def as[T]: Rep[T]
  }
  val document: Rep[Element]
  implicit def elementOps(x: Rep[Element]): ElementOps = proxyOps(x)
  implicit def canvasOps(x: Rep[Canvas]): CanvasOps = proxyOps(x)
  implicit def contextOps(x: Rep[Context]): ContextOps = proxyOps(x)
  implicit def asOps(x: Rep[_]): AsOps
}

trait DomsExp extends Doms with JSProxyExp {
  case object DocumentVar extends Exp[Element]
  val document = DocumentVar
  implicit def asOps(x: Rep[_]): AsOps = new AsOps {
    def as[T]: Rep[T] = x.asInstanceOf[Rep[T]]
  }
}

trait GenDoms extends JSGenProxy {
  val IR: DomsExp
  import IR._
  override def quote(x: Exp[Any]) : String = x match {
    case DocumentVar => "document"
    case _ => super.quote(x)
  }
}
