package scala.js

import scala.language.experimental.macros
import scala.virtualization.lms.common._
import java.io.PrintWriter
import gen.js.{GenEffect, GenDebug, GenAdts, GenListOps}
import language.{Debug, Adts}
import exp.{DebugExp, AdtsExp}

class TestAdts extends FileDiffSuite {

  val prefix = "test-out/"

  def testProducts(): Unit = {

    trait Prog extends Adts {
      import Adts.adt

      @adt case class Product(x: Int, s: String)
      @adt case class NestedProduct(p: Product, b: Boolean)

      // smart constructors
      def construction(x: Rep[Int], s: Rep[String], b: Rep[Boolean]): Rep[NestedProduct] = {
        val p = Product(x, s)
        NestedProduct(p, b)
      }

      // ops
      import NestedProduct._ // TODO Get rid of this import (SI-7073)
      def memberSelection(n: Rep[NestedProduct]) = n.p

      def equal(n1: Rep[NestedProduct], n2: Rep[NestedProduct]) = n1 === n2

      def copy(n: Rep[NestedProduct], b: Rep[Boolean]) = n.copy(n.p, b = b)

    }

    withOutFile(prefix + "adt/product") {
      val prog = new Prog with AdtsExp
      val gen = new GenAdts { val IR: prog.type = prog }
      val out = new PrintWriter(System.out)
      gen.emitSource3(prog.construction, "construction", out)
      gen.emitSource(prog.memberSelection, "selection", out)
      gen.emitSource2(prog.equal, "equal", out)
      gen.emitSource2(prog.copy, "copy", out)
    }
    assertFileEqualsCheck(prefix + "adt/product")
  }
//
//  def testSums(): Unit = {
//
//    trait Prog extends Adts {
//
//      @adt sealed trait CoProduct
//      @adt case class Left(x: Int) extends CoProduct
//      @adt case class Right(s: String) extends CoProduct
//
//      def construction1(x: Rep[Int]) = Left(x)
//
//      def construction2(s: Rep[String]) = Right(s)
//
//      def selection(l: Rep[Left]) = l.x
//
//      def equal1(c1: Rep[CoProduct], c2: Rep[CoProduct]) = c1 === c2
//
//      def equal2(c: Rep[CoProduct], l: Rep[Left]) = c === l
//
//      def copy(l: Rep[Left], x: Rep[Int]) = l.copy(x = x)
//
//      def fold(c: Rep[CoProduct]) = c.fold(
//        (l: Rep[Left]) => unit("left"),
//        (r: Rep[Right]) => unit("right")
//      )
//
//    }
//
//    withOutFile(prefix + "adt/sum") {
//      val prog = new Prog with AdtsExp
//      val gen = new GenAdts { val IR: prog.type = prog }
//      val out = new PrintWriter(System.out)
//      gen.emitSource(prog.construction1, "construction1", out)
//      gen.emitSource(prog.construction2, "construction2", out)
//      gen.emitSource(prog.selection, "selection", out)
//      gen.emitSource2(prog.equal1, "equal1", out)
//      gen.emitSource2(prog.equal2, "equal2", out)
//      out.println("ERROR === is not correct")
//      gen.emitSource2(prog.copy, "copy", out)
//      gen.emitSource(prog.fold, "fold", out)
//    }
//    assertFileEqualsCheck(prefix + "adt/sum")
//  }
//
//  def testHierarchy(): Unit = {
//    trait Prog extends Adts {
//
//      @adt sealed trait Top
//      @adt case class One(x: Int) extends Top
//      @adt sealed trait Middle extends Top
//      @adt case class Two(s: String) extends Middle
//      @adt case class Three(b: Boolean) extends Middle
//
//      def construction1(x: Rep[Int]) = One(x)
//
//      def construction2(s: Rep[String]) = Two(s)
//
//      def construction3(b: Rep[Boolean]) = Three(b)
//
//      def equal(t1: Rep[Top], t2: Rep[Top]) = t1 === t2
//
//      def fold1(t: Rep[Top]) = t.fold(
//        (o: Rep[One]) => unit("one"),
//        (t: Rep[Three]) => unit("three"),
//        (t: Rep[Two]) => unit("two")
//      )
//
//      /*def fold2(m: Rep[Middle]) = C.Middle.fold(m)(
//        (t: Rep[Three]) => unit("three"),
//        (t: Rep[Two]) => unit("two")
//      )*/
//
//    }
//
//    withOutFile(prefix + "adt/hierarchy") {
//      val prog = new Prog with AdtsExp
//      val gen = new GenAdts { val IR: prog.type = prog }
//      val out = new PrintWriter(System.out)
//      gen.emitSource(prog.construction1, "construction1", out)
//      gen.emitSource(prog.construction2, "construction2", out)
//      gen.emitSource(prog.construction3, "construction3", out)
//      gen.emitSource2(prog.equal, "equal", out)
//      out.println("ERROR === is not correct")
//      gen.emitSource(prog.fold1, "fold1", out)
//      /*gen.emitSource(prog.fold2, "fold2", out)
//      out.println("ERROR generated array for fold is not correct")*/
//    }
//    assertFileEqualsCheck(prefix + "adt/hierarchy")
//  }
//
//  def testAdt() {
//
//    val prefix = "test-out/"
//
//    trait DSL extends Base with Adts with ListOps with Debug  //need functions here
//    trait DSLExp extends DSL with AdtsExp with ListOpsExp with DebugExp
//    trait DSLJSGen extends GenEffect with GenAdts with GenListOps with GenDebug { val IR: DSLExp }
//
//    trait Prog extends DSL {
//
//      @adt case class Power(effect: String)
//
//      @adt sealed trait Person
//
//      @adt sealed trait SuperHero extends Person
//      @adt case class Mutant(name: String, saveTheWorld: Boolean, powers: List[Power]) extends SuperHero
//      @adt sealed trait Magical extends SuperHero
//      @adt case class God(name: String, good: Boolean, religion: Boolean, powers: List[Power]) extends Magical
//      @adt case class Devil(name: String, bad: Boolean, cult: Boolean, powers: List[Power]) extends Magical
//
//      @adt sealed trait Human extends Person
//      @adt case class Man(name: String, age: Int, wife: Person) extends Human
//      @adt case class Woman(name: String, age: Int, husband: Person, children: Person) extends Human
//
//      def main(n: Rep[String]) = {
//
//        val spideyWeb = Power(unit("web"))
//        val spideySens = Power(unit("spider sens"))
//        val godPower = Power(unit("all"))
//        val minosPower = Power(unit("judge of the dead"))
//
//        val spidey = Mutant(unit("SpiderMan"), unit(true), List(spideyWeb, spideySens))
//        log(spidey)
//
//        val venom = spidey.copy(unit("Venom"), unit(false), spidey.powers)
//        log(venom === spidey)
//
//        val zeus = God(unit("Zeus"), unit(true), unit(true), List(godPower))
//
//        val minos = Devil(unit("Minos"), unit(false), unit(false), List(minosPower))
//
//        val europe = Woman(unit("Europe"), unit(36), zeus, minos)
//
//        val asterion  = Man(unit("Asterion"), unit(42), europe)
//
//        log(asterion.wife)
//
//        def hello(p: Rep[Person]) = p.fold(
//          (d: Rep[Devil]) => log(d.name),
//          (g: Rep[God]) => log(g.name),
//          (m: Rep[Man]) => log(m.name),
//          (m: Rep[Mutant]) => log(m.name),
//          (w: Rep[Woman]) => log(w.name)
//        )
//
//        log(hello(minos))
//
//       }
//     }
//
//     withOutFile(prefix+"adt/test") {
//      val prog = new Prog with DSLExp
//      val codegen = new DSLJSGen { val IR: prog.type = prog }
//      codegen.emitSource(prog.main _, "main", new PrintWriter(System.out))
//      println("fold order is wrong")
//     }
//     assertFileEqualsCheck(prefix+"adt/test")
//
//  }
}
            