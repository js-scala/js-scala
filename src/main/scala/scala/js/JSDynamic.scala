package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait DynamicBase extends Base {
  type DynamicRep <: DynamicRepImpl with Rep[Any]
  trait DynamicRepImpl extends Dynamic {
    def applyDynamic(field: String): ApplyDynamicSelector
    def selectDynamic(field: String): DynamicRep
  }
  trait ApplyDynamicSelector {
    def apply(args: Rep[Any]*): DynamicRep
    def update(value: Rep[Any]): Rep[Unit]
  }
  def dynamic(x: Rep[Any]): DynamicRep
  def newDynamic(constructor: String)(args: Rep[Any]*): DynamicRep
  def inlineDynamic(code: String): DynamicRep
}

trait DynamicExp extends DynamicBase with EffectExp {
  
  type DynamicRep = DynamicExp

  case class DynamicCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicSelect(receiver: Exp[Any], field: String) extends Def[Any]
  case class DynamicUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]
  case class DynamicNew(constructor: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicInline(code: String) extends Def[Any]

  case class ApplyDynamicSelectorImpl(receiver: Exp[Any], field: String) extends ApplyDynamicSelector {
    override def apply(args: Exp[Any]*): DynamicExp =
      dynamic(reflectEffect(DynamicCall(receiver, field, args.toList)))

    override def update(value: Exp[Any]): Exp[Unit] =
      reflectEffect(DynamicUpdate(receiver, field, value))
  }

  case class DynamicExp(receiver: Exp[Any]) extends Exp[Any] with DynamicRepImpl {
    override def applyDynamic(field: String) =
      ApplyDynamicSelectorImpl(receiver, field)

    override def selectDynamic(field: String): DynamicExp =
      // No call to reflectEffect at the moment because selecting a
      // field is not _really_ a side-effecting operation. However, we
      // might still want to express that this operation causes a read
      // on the field of the receiver (TODO before attempting
      // optimizations).
      dynamic(DynamicSelect(receiver, field))
  }

  def dynamic(x: Exp[Any]) = DynamicExp(x)

  def newDynamic(constructor: String)(args: Exp[Any]*): DynamicExp =
    dynamic(reflectEffect(DynamicNew(constructor, args.toList), Alloc))

  def inlineDynamic(code: String): DynamicExp =
    dynamic(reflectEffect(DynamicInline(code)))
}

trait JSGenDynamic extends JSGenEffect {
  val IR: DynamicExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DynamicCall(receiver, method, args) =>  emitValDef(sym, 
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case DynamicSelect(receiver, field) => emitValDef(sym,
      quote(receiver) + "." + field)
    case DynamicUpdate(receiver, field, value) => emitValDef(sym,
      quote(receiver) + "." + field + " = " + quote(value))
    case DynamicNew(constructor, args) => emitValDef(sym,
      "new " + constructor + args.map(quote).mkString("(", ",", ")"))
    case DynamicInline(code) => emitValDef(sym, code)
    case _ => super.emitNode(sym, rhs)
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case DynamicExp(receiver) => quote(receiver)
    case _ => super.quote(x)
  }
}
