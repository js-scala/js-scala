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

      case class Product(x: Int, s: String) extends Adt
      case class NestedProduct(p: Product, b: Boolean) extends Adt

      // Smart constructors
      object C {
        val Product = adt[Product]
        val NestedProduct = adt[NestedProduct]
      }

      // Methods
      implicit def ProductOps(p: Rep[Product]) = adtOps(p)
      implicit def NestedProductOps(n: Rep[NestedProduct]) = adtOps(n)

      def construction(x: Rep[Int], s: Rep[String], b: Rep[Boolean]): Rep[NestedProduct] = {
        val p = C.Product(x, s)
        C.NestedProduct(p, b)
      }

      def memberSelection(n: Rep[NestedProduct]) = n.p

      def equal(n1: Rep[NestedProduct], n2: Rep[NestedProduct]) = n1 === n2

      def copy(n: Rep[NestedProduct], b: Rep[Boolean]) = n.copy(p = n.p, b = b)

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

  def testSums(): Unit = {

    trait Prog extends Adts {

      sealed trait CoProduct extends Adt
      case class Left(x: Int) extends CoProduct
      case class Right(s: String) extends CoProduct

      object C {
        val Left = adt[Left]
        val Right = adt[Right]
      }

      implicit def CoProductOps(c: Rep[CoProduct]) = adtOps(c)
      implicit def LeftOps(l: Rep[Left]) = adtOps(l)
      implicit def RightOps(r: Rep[Right]) = adtOps(r)

      def construction1(x: Rep[Int]) = C.Left(x)

      def construction2(s: Rep[String]) = C.Right(s)

      def selection(l: Rep[Left]) = l.x

      def equal1(c1: Rep[CoProduct], c2: Rep[CoProduct]) = c1 === c2

      def equal2(c: Rep[CoProduct], l: Rep[Left]) = c === l

      def copy(l: Rep[Left], x: Rep[Int]) = l.copy(x = x)

      def fold(c: Rep[CoProduct]) = c.fold(
        (l: Rep[Left]) => unit("left"),
        (r: Rep[Right]) => unit("right")
      )

    }

    withOutFile(prefix + "adt/sum") {
      val prog = new Prog with AdtsExp
      val gen = new GenAdts { val IR: prog.type = prog }
      val out = new PrintWriter(System.out)
      gen.emitSource(prog.construction1, "construction1", out)
      gen.emitSource(prog.construction2, "construction2", out)
      gen.emitSource(prog.selection, "selection", out)
      gen.emitSource2(prog.equal1, "equal1", out)
      gen.emitSource2(prog.equal2, "equal2", out)
      out.println("ERROR === is not correct")
      gen.emitSource2(prog.copy, "copy", out)
      gen.emitSource(prog.fold, "fold", out)
    }
    assertFileEqualsCheck(prefix + "adt/sum")
  }


  def testHierarchy(): Unit = {
    trait DSL extends Adts {

      sealed trait Top extends Adt
      case class One(x: Int) extends Top
      sealed trait Middle extends Top
      case class Two(s: String) extends Middle
      case class Three(b: Boolean) extends Middle

      object C {
        val One = adt[One]
        val Two = adt[Two]
        val Three = adt[Three]
      }

      implicit def TopOps(t: Rep[Top]) = adtOps(t)
      implicit def OneOps(o: Rep[One]) = adtOps(o)
      implicit def MiddleOps(m: Rep[Middle]) = adtOps(m)
      implicit def TwoOps(t: Rep[Two]) = adtOps(t)
      implicit def ThreeOps(t: Rep[Three]) = adtOps(t)
    }

    trait Prog extends DSL {

      def construction1(x: Rep[Int]) = C.One(x)

      def construction2(s: Rep[String]) = C.Two(s)

      def construction3(b: Rep[Boolean]) = C.Three(b)

      def equal(t1: Rep[Top], t2: Rep[Top]) = t1 === t2

      def fold1(t: Rep[Top]) = t.fold(
        (o: Rep[One]) => unit("one"),
        (t: Rep[Two]) => unit("two"),
        (t: Rep[Three]) => unit("three")
      )

      def fold2(m: Rep[Middle]) = m.fold(
        (t: Rep[Two]) => unit("two"),
        (t: Rep[Three]) => unit("three")
      )

    }

    withOutFile(prefix + "adt/hierarchy") {
      val prog = new Prog with AdtsExp
      val gen = new GenAdts { val IR: prog.type = prog }
      val out = new PrintWriter(System.out)
      gen.emitSource(prog.construction1, "construction1", out)
      gen.emitSource(prog.construction2, "construction2", out)
      gen.emitSource(prog.construction3, "construction3", out)
      gen.emitSource2(prog.equal, "equal", out)
      out.println("ERROR === is not correct")
      gen.emitSource(prog.fold1, "fold1", out)
      gen.emitSource(prog.fold2, "fold2", out)
    }
    assertFileEqualsCheck(prefix + "adt/hierarchy")
  }

  def testAdt() {

    val prefix = "test-out/"

    trait DSL extends Base with Adts with ListOps with Debug  //need functions here
    trait DSLExp extends DSL with AdtsExp with ListOpsExp with DebugExp
    trait DSLJSGen extends GenEffect with GenAdts with GenListOps with GenDebug { val IR: DSLExp }

    trait Prog extends DSL {
      
      case class Power(effect: String) extends Adt
      
      sealed trait Person extends Adt
      
      sealed trait SuperHero extends Person
      case class Mutant(name: String, saveTheWorld: Boolean, powers: List[Power]) extends SuperHero
      sealed trait Magical extends SuperHero
      case class God(name: String, good: Boolean, religion: Boolean, powers: List[Power]) extends Magical
      case class Devil(name: String, bad: Boolean, cult: Boolean, powers: List[Power]) extends Magical
      
      sealed trait Human extends Person
      case class Man(name: String, age: Int, wife: Person) extends Human
      case class Woman(name: String, age: Int, husband: Person, children: Person) extends Human
      
      
      implicit def powerOps(p:Rep[Power]) = adtOps(p)
      implicit def personOps(p:Rep[Person]) = adtOps(p)
      implicit def superHeroOps(sh:Rep[SuperHero]) = adtOps(sh)
      implicit def magicalOps(m:Rep[Magical]) = adtOps(m)
      implicit def humanOps(h:Rep[Human]) = adtOps(h)
      
      implicit def mutantOps(m:Rep[Mutant]) = adtOps(m)
      implicit def godOps(g:Rep[God]) = adtOps(g)
      implicit def devilOps(d:Rep[Devil]) = adtOps(d)
      implicit def manOps(m:Rep[Man]) = adtOps(m)
      implicit def womanOps(w:Rep[Woman]) = adtOps(w)
      
      
      def main(n: Rep[String]) = {
        
        val Power = adt[Power]
        val spideyWeb = Power(unit("web"))
        val spideySens = Power(unit("spider sens"))
        val godPower = Power(unit("all"))
        val minosPower = Power(unit("judge of the dead"))
        
        val Mutant = adt[Mutant]
        val spidey = Mutant(unit("SpiderMan"), unit(true), List(spideyWeb, spideySens))
        log(spidey)
        
        val venom = spidey.copy(unit("Venom"), unit(false), spidey.powers)
        log(venom === spidey)
        
        val God = adt[God]
        val zeus = God(unit("Zeus"), unit(true), unit(true), List(godPower))
        
        val Devil = adt[Devil]
        val minos = Devil(unit("Minos"), unit(false), unit(false), List(minosPower))
        
        val Woman = adt[Woman]
        val europe = Woman(unit("Europe"), unit(36), zeus, minos)
        
        val Man = adt[Man]
        val asterion  = Man(unit("Asterion"), unit(42), europe)
        
        log(asterion.wife)
        
        def hello(p: Rep[Person]) = p.fold(
          (m: Rep[Mutant]) => log(m.name),
          (g: Rep[God]) => log(g.name),
          (d: Rep[Devil]) => log(d.name),
          (m: Rep[Man]) => log(m.name),
          (w: Rep[Woman]) => log(w.name)
        )
        
        log(hello(minos))
        
       }
     }

     withOutFile(prefix+"adt/test") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main _, "main", new PrintWriter(System.out))
     }
     assertFileEqualsCheck(prefix+"adt/test")
     
  }
}
            