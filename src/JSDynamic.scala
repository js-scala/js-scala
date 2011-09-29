import scala.virtualization.lms.common._

import java.io.PrintWriter

trait DynamicBase extends Base {
  protected type DynamicRep <: DynamicRepImpl with Rep[Any]
  protected trait DynamicRepImpl extends Dynamic {
    def applyDynamic(method: String)(args: Rep[Any]*): DynamicRep
    def selectDynamic(field: String): DynamicRep
  }
  protected def dynamic(x: Rep[Any]): DynamicRep
}

trait DynamicExp extends DynamicBase with EffectExp {
  
  type DynamicRep = DynamicExp

  case class DynamicCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicSelect(receiver: Exp[Any], field: String) extends Def[Any]
  
  case class DynamicExp(receiver: Exp[Any]) extends Exp[Any] with DynamicRepImpl {
    override def applyDynamic(method: String)(args: Exp[Any]*): DynamicExp =
      dynamic(reflectEffect(DynamicCall(receiver, method, args.toList)))

    override def selectDynamic(field: String): DynamicExp =
      //no call to reflectEffect at the moment because selecting field is not sideeffecting operation
      //probably we need some other way of expressing the effect (like reading effect)
      dynamic(DynamicSelect(receiver, field))
  }
  
  def dynamic(x: Exp[Any]) = DynamicExp(x)

}

trait JSGenDynamic extends JSGenEffect {
  val IR: DynamicExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DynamicCall(receiver, method, args) =>  emitValDef(sym, 
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case DynamicSelect(receiver, field) =>
      emitValDef(sym, quote(receiver) + "." + field)
    case _ => super.emitNode(sym, rhs)
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case DynamicExp(receiver) => quote(receiver)
    case _ => super.quote(x)
  }
}