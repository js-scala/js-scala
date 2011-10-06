import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait Prog1 { this: LiftNumeric with NumericOps with Arrays =>
  def test(x: Rep[Int]): Rep[Int] = {
    val a = array(1, 2, 3)
    a(1) = 4
    a.length + a(1) + x
  }
}

class TestArrays extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testArrays = {
    withOutFile(prefix+"arrays") {
    
      println("-- begin 1")
      new Prog1 with LiftNumeric with NumericOpsExpOpt with ArraysExp { self =>
        val codegen = new JSGenNumericOps with JSGenArrays { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
      println("-- end 1")

    }
    assertFileEqualsCheck(prefix+"arrays")
  }
}
