package scala.js

import scala.util.continuations._
import scala.virtualization.lms.common._

trait CPS extends JS with LiftVariables with Arrays {

  implicit def richArray[A: Manifest](xs: Rep[Array[A]]): SuspendableArrayOps[A] =
    new SuspendableArrayOps(xs)

  class SuspendableArrayOps[A: Manifest](xs: Rep[Array[A]]) extends java.io.Serializable {
    def suspendable = new java.io.Serializable {
      def foreach(yld: Rep[A] => Rep[Unit] @suspendable): Rep[Unit] @suspendable = {
        var i = 0
        suspendableWhile(i < xs.length, { yld(xs(i)); i += 1; unit(()) })
      }
      def map[B: Manifest](f: Rep[A] => Rep[B] @suspendable): Rep[Array[B]] @suspendable = {
        // val ys = array[B]()
        // var i = 0
        // for (x <- xs.sus) { ys(i) = f(x); i += 1 }
        // ys
        ???
      }
    }
  }

  type suspendable = cps[Rep[Unit]]

  def suspendableWhile(cond: => Rep[Boolean], body: => Rep[Unit] @suspendable): Rep[Unit] @suspendable

}

trait CPSExp extends CPS {

  def suspendableWhile(cond: => Rep[Boolean], body: => Rep[Unit] @suspendable): Rep[Unit] @suspendable = shift { k: (Rep[Unit] => Rep[Unit]) =>
    lazy val rec: Rep[Unit => Unit] = fun { () => if (cond) reset { body; rec() } else k() }
    rec()
  }

}
