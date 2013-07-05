package scala.js.exp

import scala.js.language.StateOps
import scala.virtualization.lms.common._

trait StateOpsExp extends StateOps with VariablesExp with TupleOpsExp {

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