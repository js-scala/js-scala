import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait UnitProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    // should return undefined
  }
}

class TestUnit extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testUnit = {
    withOutFile(prefix+"unit") {
    
      println("-- begin")

      new UnitProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        val f = (x: Rep[Any]) => test(x)
        codegen.emitSource(f, "main", new PrintWriter(System.out))
      }

      println("-- end")
    }
    assertFileEqualsCheck(prefix+"unit")
  }
}
