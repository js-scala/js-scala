package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait WhileProg { this: JS with LiftVariables =>
  def test(n: Rep[Int]): Rep[Int] = {
    var x = 0
    while(x < n) { x += 1 }
    x
  }
}

trait SplitJoinProg { this: JS =>
  def test(s: Rep[String]): Rep[String] = {
    (for (x <- s.split(" ")) yield (x + 1)).join(" ")
  }
}

trait ObjectProg { this: JS =>
  def test(o: Rep[Any]): Rep[String] = o.toString() + unit(42).toString()
}

class TestCompile extends FileDiffSuite {
  val prefix = "test-out/"
  
  def testWhile = {
    withOutFile(prefix+"while") {
      new WhileProg with JSExp with LiftVariables { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"while")
  }

  def testSplitJoin = {
    withOutFile(prefix+"splitjoin") {
      new SplitJoinProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"splitjoin")
  }

  def testObject = {
    withOutFile(prefix+"object") {
      new ObjectProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(test _, "main", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"object")
  }

}
