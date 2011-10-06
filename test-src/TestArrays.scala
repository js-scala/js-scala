import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait Prog { this: LiftNumeric with NumericOps with Arrays =>
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
}

class TestArrays extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testArrays = {
    withOutFile(prefix+"arrays") {
    
      new Prog with LiftNumeric with NumericOpsExpOpt with ArraysExp { self =>
        val codegen = new JSGenNumericOps with JSGenArrays { val IR: self.type = self }

	println("-- begin 1")
        codegen.emitSource(test1 _, "main", new PrintWriter(System.out))
        println("-- end 1")

	println("-- begin 2")
        codegen.emitSource(test2 _, "main", new PrintWriter(System.out))
        println("-- end 2")

      }

    }
    assertFileEqualsCheck(prefix+"arrays")
  }
}
