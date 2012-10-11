package scala.js

import virtualization.lms.common._

trait StateOps { this: Base =>

  // type “State” is already defined in Effects :-(
  type StateM[S, +A]
  class StateMOps[S, A](s: StateM[S, A]) {
    def map[B](f: Rep[A] => Rep[B]) = state_map(s, f)
    def flatMap[B](f: Rep[A] => StateM[S, B]) = state_flatMap(s, f)
    def apply(zero: Rep[S])(implicit mS: Manifest[S], mA: Manifest[A]) = state_apply(s, zero)
    def run(zero: Rep[S])(implicit mS: Manifest[S], mA: Manifest[A]) = apply(zero)
    def eval(zero: Rep[S])(implicit mS: Manifest[S], mA: Manifest[A]) = state_eval(s, zero)
    def exec(zero: Rep[S])(implicit mS: Manifest[S], mA: Manifest[A]) = state_exec(s, zero)
  }
  implicit def StateMToOps[S, A](s: StateM[S, A]) = new StateMOps(s)

  object State {
    def modify[S](f: Rep[S] => Rep[S]) = state_modify(f)
    def get[S] = state_get[S]
    def put[S](s: Rep[S]) = state_put(s)
    def state[S] = new StateFunctionsState[S]
    class StateFunctionsState[S] {
      def apply[A](a: Rep[A]) = state_state[S, A](a)
    }
  }

  def state_map[S, A, B](s: StateM[S, A], f: Rep[A] => Rep[B]): StateM[S, B]
  def state_flatMap[S, A, B](s: StateM[S, A], f: Rep[A] => StateM[S, B]): StateM[S, B]
  def state_modify[S](f: Rep[S] => Rep[S]): StateM[S, Unit]
  def state_get[S]: StateM[S, S]
  def state_put[S](s: Rep[S]): StateM[S, Unit]
  def state_state[S, A](a: Rep[A]): StateM[S, A]

  def state_apply[S : Manifest, A : Manifest](s: StateM[S, A], zero: Rep[S]): Rep[(S, A)]
  def state_eval[S : Manifest, A : Manifest](s: StateM[S, A], zero: Rep[S]): Rep[A]
  def state_exec[S : Manifest, A : Manifest](s: StateM[S, A], zero: Rep[S]): Rep[S]
}

trait StateOpsExp extends StateOps { this: VariablesExp with TupleOpsExp =>

  def state_map[S, A, B](s: StateM[S, A], f: Exp[A] => Exp[B]) = StateMapOp(s, f)
  def state_flatMap[S, A, B](s: StateM[S, A], f: Exp[A] => StateM[S, B]) = StateFlatMapOp(s, f)
  def state_modify[S](f: Exp[S] => Exp[S]) = StateModifyOp(f)
  def state_get[S] = StateGetOp[S]()
  def state_put[S](s: Exp[S]) = StatePutOp(s)
  def state_state[S, A](a: Rep[A]) = StateStateOp[S, A](a)

  def state_apply[S : Manifest, A : Manifest](sM: StateM[S, A], zero: Exp[S]) = {
    val s = var_new(zero)
    val a = stateFold(sM, s)
    (readVar(s), a)
  }
  def state_eval[S : Manifest, A : Manifest](sM: StateM[S, A], zero: Exp[S]) = {
    val s = var_new(zero)
    stateFold(sM, s)
  }
  def state_exec[S : Manifest, A : Manifest](sM: StateM[S, A], zero: Exp[S]) = {
    val s = var_new(zero)
    stateFold(sM, s)
    s
  }

  // Fold effects of a StateM computation
  private def stateFold[S : Manifest, A : Manifest](sM: StateM[S, A], s: Var[S]): Exp[A] = sM match {
    case StateMapOp(sM, f) =>
      val a = stateFold(sM, s) // FIXME As sM has type Exp[StateM[S, *Any*]] some typing issues may rise
      val b = reifyEffects(f(a))
      StateBlock(b)
    case StateFlatMapOp(sM, f) =>
      val a0 = stateFold(sM, s) // FIXME As sM has type Exp[StateM[S, *Any*]] some typing issues may rise
      val a = stateFold(f(a0), s)
      val b = reifyEffects(a)
      StateBlock(b)
    case StateGetOp() =>
      readVar(s.asInstanceOf[Var[A]])
    case StatePutOp(value) =>
      var_assign(s, value).asInstanceOf[Exp[A]]
    case StateModifyOp(f) =>
      val b = reifyEffects(f(s))
      var_assign(s, StateBlock(b)).asInstanceOf[Exp[A]]
    case StateStateOp(a) =>
      a
  }

  sealed trait StateM[S, +A]
  case class StateMapOp[S, A, B](sM: StateM[S, A], f: Exp[A] => Exp[B]) extends StateM[S, B]
  case class StateFlatMapOp[S, A, B](sM: StateM[S, A], f: Exp[A] => StateM[S, B]) extends StateM[S, B]
  case class StateModifyOp[S](f: Exp[S] => Exp[S]) extends StateM[S, Unit]
  case class StateGetOp[S]() extends StateM[S, S]
  case class StatePutOp[S](s: Exp[S]) extends StateM[S, Unit]
  case class StateStateOp[S, A](a: Exp[A]) extends StateM[S, A]

  case class StateBlock[B](b: Block[B]) extends Def[B] // FIXME Extend Exp instead of Def to avoid to emit a value which may be discarded in the rest of the program
}

trait JSGenStateOps extends JSGenEffect { this: JSGenVariables with JSGenStateOps =>
  val IR: EffectExp with StateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StateBlock(b) =>
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenStateOps extends ScalaGenEffect { this: ScalaGenVariables with ScalaGenStateOps =>
  val IR: EffectExp with StateOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StateBlock(b) =>
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
    case _ => super.emitNode(sym, rhs)
  }
}