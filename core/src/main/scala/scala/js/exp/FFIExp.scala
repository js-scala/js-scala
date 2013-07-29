package scala.js.exp

import scala.virtualization.lms.common.EffectExp

trait FFIExp extends EffectExp {

  implicit class ForeignInterpolator(ctx: StringContext) {
    def foreign(args: Exp[Any]*): ForeignExpressionBuilder =
      ForeignExpressionBuilder(ctx, args)
  }

  case class ForeignExpressionBuilder(ctx: StringContext, args: Seq[Exp[Any]]) {
    def withEffect[A : Manifest](effect: Summary = Simple()) = ForeignExpression(ctx, args) withEffect effect
    def pure[A : Manifest] = apply[A]
    def apply[A : Manifest] = ForeignExpression(ctx, args)
  }

  case class ForeignExpression[A : Manifest](ctx: StringContext, args: Seq[Exp[Any]]) extends Def[A] {
    def withEffect(effect: Summary = Simple()) = reflectEffect(this, effect)
  }

}
