import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait DynamicProg { this: DynamicBase =>
  def test(x: Rep[Any]): Rep[Any] = {
    dynamic(x).foo().bar(x)
  }
}

class TestDynamic extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testDynamic = {
    withOutFile(prefix+"dynamic") {
    
      println("-- begin")

      new DynamicProg with DynamicExp { self =>
        val codegen = new JSGenDynamicCall { val IR: self.type = self }
        val f = (x: Rep[Any]) => test(x)
        codegen.emitSource(f, "main", new PrintWriter(System.out))
      }

      println("-- end")
    }
    assertFileEqualsCheck(prefix+"dynamic")
  }
}
