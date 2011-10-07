import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait DynamicProg { this: DynamicBase =>
  def test(x: Rep[Any]): Rep[Any] = {
    dynamic(x).foo.bar(x).baz()
  }
}

trait AllocProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    val f = fun { y : Rep[Any] => new JSLiteral { val a = newDynamic("Foo")() } }
    f(x)
  }
}

class TestDynamic extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testDynamic = {
    withOutFile(prefix+"dynamic") {
      new DynamicProg with DynamicExp { self =>
        val codegen = new JSGenDynamic { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"dynamic")
  }

  def testAlloc = {
    withOutFile(prefix+"dynamic_alloc") {
      new AllocProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"dynamic_alloc")
  }
}
