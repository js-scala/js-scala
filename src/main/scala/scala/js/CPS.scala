package scala.js

import scala.util.continuations._
import scala.virtualization.lms.common._

import java.io.PrintWriter

trait CPS extends JS with LiftVariables with Arrays with JSProxyBase {
  
  type suspendable = cps[Rep[Unit]]

  implicit def richArray[A: Manifest](xs: Rep[Array[A]]) = new java.io.Serializable {
    def suspendable: SuspendableArrayOps[A] = new SuspendableArrayOps[A](xs)
    def parSuspendable: ParSuspendableArrayOps[A] = new ParSuspendableArrayOps[A](xs)
  }

  class SuspendableArrayOps[A: Manifest](xs: Rep[Array[A]]) extends java.io.Serializable {
    def foreach(yld: Rep[A] => Rep[Unit] @suspendable): Rep[Unit] @suspendable = {
      var i = 0
      suspendableWhile(i < xs.length, { yld(xs(i)); i += 1; unit(()) })
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

  def suspendableWhile(cond: => Rep[Boolean], body: => Rep[Unit] @suspendable): Rep[Unit] @suspendable
  
  class DataFlowCell[A: Manifest](cell: Cell[A]) extends java.io.Serializable {
    def apply() = shift { k: (Rep[A] => Rep[Unit]) =>
      cell.get(fun(k))
      unit(())
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

trait CPSExp extends CPS with JSProxyExp {
  
  case class CellNode[A: Manifest]() extends Def[Cell[A]]
  
  def createCell[A: Manifest](): Rep[Cell[A]] = reflectEffect(CellNode[A]())

  def suspendableWhile(cond: => Rep[Boolean], body: => Rep[Unit] @suspendable): Rep[Unit] @suspendable = shift { k: (Rep[Unit] => Rep[Unit]) =>
    lazy val rec: Rep[Unit => Unit] = fun { () => if (cond) scala.util.continuations.reset[Rep[Unit], Rep[Unit]] { body; rec() } else k() }
    rec()
  }

}

trait GenCPS extends JSGenProxy {
  val IR: CPSExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case CellNode() => emitValDef(sym, "new Cell()")
    case _ => super.emitNode(sym, rhs)
  }
}
