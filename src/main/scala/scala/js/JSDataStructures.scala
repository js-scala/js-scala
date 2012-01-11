package scala.js

import scala.virtualization.lms.common._

trait JSDataStructures extends JS with JSClasses {
  def array_push[T:Manifest](a: Rep[Array[T]], x: Rep[T]) = a(a.length) = x

  class Cell[A:Manifest] {
    private var value = unit(null).asInstanceOf[Rep[A]]
    private var defined = unit(false)
    private val queue = array[A => Unit]()

    def get(k: Rep[A => Unit]) = {
      if (defined) k(value)
      else array_push(queue, k)
    }

    def set(v: Rep[A]) = {
      if (defined) () // error
      else {
        value = v
        defined = true
        for (f <- queue) { f(v) } // spawn
      }
    }
  }
}

trait JSDataStructuresExp extends JSDataStructures with JSExp with JSClassesExp

trait JSGenDataStructures extends JSGen with JSGenClasses {
  val IR: JSDataStructuresExp
  import IR._
}

