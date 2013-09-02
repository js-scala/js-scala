package scala.js

import scala.language.experimental.macros
import scala.virtualization.lms.common._
import java.io.PrintWriter
import gen.js.{GenEffect, GenDebug, GenAdts, GenListOps}
import language.{Debug, Adts}
import exp.{DebugExp, AdtsExp}

class TestAdt extends FileDiffSuite {

  def testAdt() {

    val prefix = "test-out/"

    trait DSL extends Base with Adts with ListOps with Functions with Debug  //need functions here
    trait DSLExp extends DSL with AdtsExp with ListOpsExp with FunctionsExp with DebugExp
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
        
        
        def hello(p: Rep[Person]) = p.Fold(
            (m: Rep[Mutant]) => log(m.name),
            (g: Rep[God]) => log(g.name),
            (d: Rep[Devil]) => log(d.name),
            (m: Rep[Man]) => log(m.name),
            (w: Rep[Woman]) => log(w.name)
            )
        
        log(hello(minos))
        
       }
     }

     withOutFile(prefix+"adt") {
      val prog = new Prog with DSLExp
      val codegen = new DSLJSGen { val IR: prog.type = prog }
      codegen.emitSource(prog.main _, "main", new PrintWriter(System.out))
     }
     assertFileEqualsCheck(prefix+"adt")
     
  }
}
            