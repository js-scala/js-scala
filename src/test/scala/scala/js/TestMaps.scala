package scala.js

import scala.virtualization.lms.common._
import java.io.PrintWriter
import java.io.FileOutputStream
import scala.js.language.JSMaps
import scala.js.language.JS
import scala.js.exp.JSExp
import scala.js.gen.js.GenJSMaps
import scala.js.exp.JSMapsExp
import scala.js.gen.js.GenJS

trait MapsProg { this: JS with JSMaps =>
  def test1(): Rep[Map[String, Int]] = {
    JSMap[String, Int]()
  }

  def test2(map: Rep[Map[String, Int]]): Rep[Int] = {
    map("test")
  }

  def test3(map: Rep[Map[Int, String]]): Rep[String] = {
    map(3)
  }

  def test2b(map: Rep[Map[String, Int]]): Rep[Option[Int]] = {
    map.get("test")
  }

  def test3b(map: Rep[Map[Int, String]]): Rep[Option[String]] = {
    map.get(3)
  }

  def test4(map: Rep[Map[String, Int]]): Rep[Unit] = {
    map("test") = 4
  }

  def test5(map: Rep[Map[Int, String]]): Rep[Unit] = {
    map(3) = "tmp"
  }

  def test6(map: Rep[Map[String, Int]]): Rep[Boolean] = {
    map.contains("test")
  }

  def test7(map: Rep[Map[Int, String]]): Rep[Boolean] = {
    map.contains(3)
  }

  def test8(map: Rep[Map[Int, String]]): Rep[Int] = {
    map.size
  }

  def test9(map: Rep[Map[Int, String]]): Rep[Array[String]] = {
    map.values
  }

  def test10(map: Rep[Map[Int, String]]): Rep[Unit] = {
    map.clear
  }

  def test11(map: Rep[Map[Int, String]]): Rep[Array[Int]] = {
    map.keys
  }

  def test12(map: Rep[Map[String, Int]]): Rep[Unit] = {
    map.foreach {
      case (key, value) =>
        map(key) = value + 1
    }
  }

  def test13(map: Rep[Map[String, Int]]): Rep[Map[String, Int]] = {
    map.mapValues { v =>
      v + 3
    }
  }

  def test14(map: Rep[Map[String, Int]]): Rep[Map[String, Int]] = {
    map.filter {
      case (_, value) =>
        value > 10
    }
  }

  def test15(map: Rep[Map[String, Int]]): Rep[Unit] = {
    for((key, value) <- map) {
      ()
    }
  }

}

class TestMaps extends FileDiffSuite {

  val prefix = "test-out/maps/"

  def testCreateMap = {
    withOutFile(prefix+"create") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource0(test1 _, "test1", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"create")
  }

  def testFieldAccess = {
    withOutFile(prefix+"access") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test2 _, "test2", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"access")
  }

  def testFieldAccessOpt = {
    withOutFile(prefix+"access-opt") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test2 _, "test2b", new PrintWriter(System.out))
        codegen.emitSource(test3 _, "test3b", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"access-opt")
  }

  def testFieldUpdate = {
    withOutFile(prefix+"update") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test4 _, "test4", new PrintWriter(System.out))
        codegen.emitSource(test5 _, "test5", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"update")
  }

  def testContainsField = {
    withOutFile(prefix+"contains") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test6 _, "test6", new PrintWriter(System.out))
        codegen.emitSource(test7 _, "test7", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"contains")
  }

  def testSize = {
    withOutFile(prefix+"size") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test8 _, "test8", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"size")
  }

  def testValues = {
    withOutFile(prefix+"values") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test9 _, "test9", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"values")
  }

  def testClear = {
    withOutFile(prefix+"clear") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test10 _, "test10", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"clear")
  }

  def testKeys = {
    withOutFile(prefix+"keys") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test11 _, "test11", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"keys")
  }

  def testForeach = {
    withOutFile(prefix+"foreach") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test12 _, "test12", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"foreach")
  }

  def testMapValues = {
    withOutFile(prefix+"mapValues") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test13 _, "test13", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"mapValues")
  }

  def testFilter = {
    withOutFile(prefix+"filter") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test14 _, "test14", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"filter")
  }

  def testFilterForeach = {
    withOutFile(prefix+"filter-foreach") {
      new MapsProg with JSExp with JSMapsExp { self =>
        val codegen = new GenJS with GenJSMaps { val IR: self.type = self }
        codegen.emitSource(test15 _, "test15", new PrintWriter(System.out))
      }

    }
    assertFileEqualsCheck(prefix+"filter-foreach")
  }
}
