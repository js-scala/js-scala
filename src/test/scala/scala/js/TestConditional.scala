package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait ConditionalProg { this: LiftNumeric with NumericOps with Equal with Print with IfThenElse =>
  
  def test(x: Rep[Double]): Rep[Double] = {
    
    print("yoyo")
    
    val z = if (x == x) {
      print("yoyo")
      print("xxx")
      print("yoyo")
      (x+4)
    } else {
      (x+6)
    }
    
    print("yyy")
    print("yoyo")
    
    z + (x + 4)
  }
  
}

class TestConditional extends FileDiffSuite {
  
  val prefix = "test-out/"
  
  def testConditional = {
    withOutFile(prefix+"conditional") {
    
      println("-- begin")

      new ConditionalProg with LiftNumeric with NumericOpsExpOpt with EqualExp with PrintExp with IfThenElseExp with CompileScala { self =>
        val codegen = new ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenEqual with ScalaGenPrint { val IR: self.type = self }
        val f = (x: Rep[Double]) => test(x)
        codegen.emitSource(f, "Test", new PrintWriter(System.out))
        val g = compile(f)
        println(g(7.0))
      }
    
      new ConditionalProg with LiftNumeric with NumericOpsExpOpt with EqualExp with PrintExp with IfThenElseExp { self =>
        val codegen = new JSGenIfThenElse with JSGenNumericOps with JSGenEqual with JSGenPrint { val IR: self.type = self }
        val f = (x: Rep[Double]) => test(x)
        codegen.emitSource(f, "main", new PrintWriter(System.out))
        codegen.emitHTMLPage(() => f(7.0), new PrintWriter(new FileOutputStream(prefix+"conditional.html")))
      }

      println("-- end")
    }
    assertFileEqualsCheck(prefix+"conditional")
    assertFileEqualsCheck(prefix+"conditional.html")
  }
}
