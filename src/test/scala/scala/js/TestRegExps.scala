package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait RegExpsProg { this: JS =>
  def testTest(str: Rep[String]): Rep[Boolean] = {
    val r = "[0-9.+]+".r
    r.test(str)
  }

  def testTestWhole(str: Rep[String]): Rep[Boolean] = {
    val r = "^[0-9.+]+$".r
    r.test(str)
  }

  def testSearch(str: Rep[String]): Rep[Int] = {
    val r = "[0-9.+]+".r
    str.search(r)
  }

  def testSearchMatch(str: Rep[String]): Rep[Boolean] = {
    testSearch(str) != -1
  }
}

trait RegExpsProgInScala extends RegExpsProg with JSInScala

class TestRegExps extends FileDiffSuite {
  val prefix = "test-out/"

  def testJS = {
    withOutFile(prefix+"regexps") {
      new RegExpsProg with JSExp { self =>
        val codegen = new JSGen { val IR: self.type = self }
        codegen.emitSource(testTest _, "mainTest", new PrintWriter(System.out))
        codegen.emitSource(testTestWhole _, "mainTestWhole", new PrintWriter(System.out))
        codegen.emitSource(testSearch _, "mainSearch", new PrintWriter(System.out))
        codegen.emitSource(testSearchMatch _, "mainSearchMatch", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"regexps")
  }

  def testScala = {
    new RegExpsProgInScala { self =>
      expect(true){testTest("123")}
      expect(true){testTest("x123y")}
      expect(false){testTest("xy")}

      expect(true){testTestWhole("123")}
      expect(false){testTestWhole("x123y")}
      expect(false){testTestWhole("xy")}

      expect(0){testSearch("123")}
      expect(1){testSearch("x123y")}
      expect(-1){testSearch("xy")}

      expect(true){testSearchMatch("123")}
      expect(true){testSearchMatch("x123y")}
      expect(false){testSearchMatch("xy")}
    }
  }
}
