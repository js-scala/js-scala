package scala.js

trait Doms extends DynamicBase {
  val document: DynamicRep
}

trait DomsExp extends Doms with DynamicExp {
  case object Document extends Exp[Any]
  val document = dynamic(Document)
}

trait GenDoms extends JSGenBase {
  val IR: DomsExp
  import IR._
  override def quote(x: Exp[Any]) : String = x match {
    case Document => "document"
    case _ => super.quote(x)
  }
}
