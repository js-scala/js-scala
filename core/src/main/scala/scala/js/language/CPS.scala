package scala.js.language

import scala.util.continuations._
import scala.virtualization.lms.common._

trait CPS extends JS with LiftVariables with Proxy {
  
  type suspendable = cps[Rep[Unit]]

  implicit def richArray[A: Manifest](xs: Rep[Array[A]]) = new java.io.Serializable {
    def suspendable: SuspendableArrayOps[A] = new SuspendableArrayOps[A](xs)
    def parSuspendable: ParSuspendableArrayOps[A] = new ParSuspendableArrayOps[A](xs)
  }

  class SuspendableArrayOps[A: Manifest](xs: Rep[Array[A]]) extends java.io.Serializable {
    def foreach(yld: Rep[A] => Rep[Unit] @suspendable): Rep[Unit] @suspendable = {
      var i = 0
      suspendableWhile(i < xs.length) { yld(xs(i)); i += 1 }
    }
    def map[B: Manifest](f: Rep[A] => Rep[B] @suspendable): Rep[Array[B]] @suspendable = {
      val ys = array[B]()
      var i = 0
      for (x <- xs.suspendable) { ys(i) = f(x); i += 1 }
      ys
    }
  }
  
  class ParSuspendableArrayOps[A: Manifest](xs: Rep[Array[A]]) extends java.io.Serializable {
    def foreach(yld: Rep[A] => Rep[Unit] @suspendable): Rep[Unit] @suspendable = {
      val futures = xs.map(x => future(yld(x))) // sequential list of futures
      futures.suspendable.foreach(_.apply())
    }
    def map[B: Manifest](f: Rep[A] => Rep[B] @suspendable): Rep[Array[B]] @suspendable = {
      val futures = xs.map(x => future(f(x))) // sequential list of futures
      futures.suspendable.map(_.apply())
    }
  }
  
  def spawn(body: => Rep[Unit] @suspendable): Rep[Unit] = {
    reset(body) //we might queue this in an event queue
  }
  
  def future[A: Manifest](body: => Rep[A] @suspendable) = {
    val cell = createCell[A]()
    spawn { cell.set(body) }
    cell
  }

  def suspendableWhile(cond: => Rep[Boolean])(body: => Rep[Unit] @suspendable): Rep[Unit] @suspendable = shift { k =>
    lazy val rec: Rep[Unit => Unit] = fun { () => if (cond) reset { body; rec() } else k() }
    rec()
  }
  
  class DataFlowCell[A: Manifest](cell: Cell[A]) extends java.io.Serializable {
    def apply() = shift { k: (Rep[A] => Rep[Unit]) =>
      cell.get(fun(k))
    }
    def set(v: Rep[A]): Rep[Unit] = cell.set(v)
  }
  
  implicit def pimpCell[A:Manifest](x: Rep[Cell[A]]): DataFlowCell[A] = {
    new DataFlowCell(repProxy[Cell[A]](x))
  }
  
  def createCell[A: Manifest](): Rep[Cell[A]]
  trait Cell[A] {
    def get(k: Rep[A => Unit]): Rep[Unit]
    def set(v: Rep[A]): Rep[Unit]
  }

}