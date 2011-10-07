import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait Prog { this: LiftNumeric with NumericOps with Equal with Arrays =>
  def test1(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    a(1) = 4
    a.length + a(1) + x
  }

  def test2(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    val b = array(4, 5, 6)
    for (el <- a) {
      b(0) = el
    }
    b(0)
  }

  def test3(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    val b = for (el <- a) yield el+x
    b(0)
  }

  def test4(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    val b = for (x1 <- a; x2 <- a) yield x1+x2
    b(0)
  }

  def test5(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    val b = for (el <- a; if (el == x)) yield el
    b(0)
  }
}

class TestArrays extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testArrays = {
    withOutFile(prefix+"arrays") {
    
      new Prog with LiftNumeric with NumericOpsExpOpt with EqualExp with ArraysExp { self =>
        val codegen = new JSGenNumericOps with JSGenEqual with JSGenArrays { val IR: self.type = self }
        codegen.emitSource(test1 _, "test1", new PrintWriter(System.out))
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3", new PrintWriter(System.out))
        codegen.emitSource(test4 _, "test4", new PrintWriter(System.out))
        codegen.emitSource(test5 _, "test5", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"arrays")
  }
}
