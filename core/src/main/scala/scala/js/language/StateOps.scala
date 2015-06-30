package scala.js.language

import scala.language.implicitConversions
import scala.language.higherKinds

import scala.virtualization.lms.common.Base

trait StateOps extends Base {

  // type State is already defined in Effects :-(
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
