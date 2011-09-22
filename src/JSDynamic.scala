import scala.virtualization.lms.common._

import java.io.PrintWriter

trait DynamicBase extends Base {
  protected type DynamicRep <: DynamicRepImpl with Rep[Any]
  protected trait DynamicRepImpl extends Dynamic {
    def applyDynamic(method: String)(args: Rep[Any]*): DynamicRep
  }
  protected def dynamic(x: Rep[Any]): DynamicRep
}

trait DynamicExp extends DynamicBase with EffectExp {
  
  type DynamicRep = DynamicExp

  case class DynamicCall(receiver: Rep[Any], method: String, args: List[Rep[Any]]) extends Def[Any]
  
  case class DynamicExp(receiver: Rep[Any]) extends Exp[Any] with DynamicRepImpl {
    override def applyDynamic(method: String)(args: Rep[Any]*): DynamicRep =
      dynamic(reflectEffect(DynamicCall(receiver, method, args.toList)))
  }
  
  def dynamic(x: Rep[Any]) = DynamicExp(x)

}

trait JSGenDynamicCall extends JSGenEffect {
  val IR: DynamicExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DynamicCall(receiver, method, args) =>  emitValDef(sym, 
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym, rhs)
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case DynamicExp(receiver) => quote(receiver)
    case _ => super.quote(x)
  }
}