package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream

trait Print extends Base {
  implicit def unit(s: String): Rep[String]
  def print(s: Rep[Any]): Rep[Unit]
}

trait PrintExp extends Print with EffectExp {
  implicit def unit(s: String): Rep[String] = Const(s)
  case class Print(s: Rep[Any]) extends Def[Unit]
  def print(s: Rep[Any]) = reflectEffect(Print(s))
}

trait ScalaGenPrint extends ScalaGenEffect {
  val IR: PrintExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Print(s) =>  emitValDef(sym, "println(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenPrint extends JSGenEffect {
  val IR: PrintExp
  import IR._
  
  // TODO: should have a function for this
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Print(s) =>  emitValDef(sym, "document.body.appendChild(document.createElement(\"div\"))"+
        ".appendChild(document.createTextNode("+quote(s)+"))")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ConditionalProg { this: LiftNumeric with NumericOps with Equal with Print with IfThenElse =>

  def test2(x: Rep[Double]): Rep[Double] = {
    val z = 1
    val y = z+1
    x + y
  }
    
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

trait FunProg { this: JSFunctions =>
  def test(x: Rep[Any]): Rep[Any] = {
    val id = fun { x : Rep[Any] => x }
    id(x)
  }
}

trait SomeProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    lazy val id: Rep[Any => Any] = fun { x : Rep[Any] =>
      dynamic(x).foo.bar()
      id(x)
    }
    id(x)
  }
}

object Main extends App {
  new ConditionalProg with IfThenElseExp with LiftNumeric with NumericOpsExpOpt with EqualExp with PrintExp { self =>
    val codegenScala = new ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenEqual with ScalaGenPrint { val IR: self.type = self }
    codegenScala.emitSource(test _, "Test", new PrintWriter(System.out))
    val codegenJS = new JSGenIfThenElse with JSGenNumericOps with JSGenEqual with JSGenPrint { val IR: self.type = self }
    codegenJS.emitSource(test _, "main", new PrintWriter(System.out))
    codegenJS.emitSource(test2 _, "main", new PrintWriter(System.out))
  }

  new FunProg with JSFunctionsExp { self =>
    val codegen = new JSGenFunctions { val IR: self.type = self }
    codegen.emitSource(test _, "main", new PrintWriter(System.out))
  }

  new SomeProg with JSExp { self =>
    val codegen = new JSGen { val IR: self.type = self }
    codegen.emitSource(test _, "main", new PrintWriter(System.out))
  }

  Koch.writeHtml("koch.html")
}
