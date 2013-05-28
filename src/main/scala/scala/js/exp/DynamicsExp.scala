package scala.js.exp

import scala.js.language.Dynamics
import scala.virtualization.lms.common.EffectExp

trait DynamicsExp extends Dynamics with EffectExp {
  
  type DynamicRep = DynamicExp

  case class DynamicCall(receiver: Exp[Any], method: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicSelect(receiver: Exp[Any], field: String) extends Def[Any]
  case class DynamicUpdate(receiver: Exp[Any], field: String, value: Exp[Any]) extends Def[Unit]
  case class DynamicNew(constructor: String, args: List[Exp[Any]]) extends Def[Any]
  case class DynamicInline(code: String) extends Def[Any]

  case class ApplyDynamicSelectorImpl(receiver: Exp[Any], field: String) extends ApplyDynamicSelector {
    override def apply(args: Exp[Any]*): DynamicExp =
      dynamic(reflectEffect(DynamicCall(receiver, field, args.toList)))
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

    override def updateDynamic(field: String)(value: Exp[Any]): Exp[Unit] =
      reflectEffect(DynamicUpdate(receiver, field, value))
  }

  def dynamic(x: Exp[Any]) = DynamicExp(x)

  def newDynamic(constructor: String)(args: Exp[Any]*): DynamicExp =
    dynamic(reflectEffect(DynamicNew(constructor, args.toList)))

  def inlineDynamic(code: String): DynamicExp =
    dynamic(reflectEffect(DynamicInline(code)))
}