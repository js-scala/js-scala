package scala.js

import java.io.PrintWriter

import scala.js.exp.JSExp
import scala.js.gen.js.GenJS
import scala.js.language.JS
import scala.virtualization.lms.common._

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
        val codegen = new GenJS { val IR: self.type = self }
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
      expectResult(true){testTest("123")}
      expectResult(true){testTest("x123y")}
      expectResult(false){testTest("xy")}

      expectResult(true){testTestWhole("123")}
      expectResult(false){testTestWhole("x123y")}
      expectResult(false){testTestWhole("xy")}

      expectResult(0){testSearch("123")}
      expectResult(1){testSearch("x123y")}
      expectResult(-1){testSearch("xy")}

      expectResult(true){testSearchMatch("123")}
      expectResult(true){testSearchMatch("x123y")}
      expectResult(false){testSearchMatch("xy")}
    }
  }
}
