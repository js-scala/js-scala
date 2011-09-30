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



trait Dom extends Base {
  // not used yet...
  type DOMObjectInternal
  type DOMObject = Rep[DOMObjectInternal]
  def document: DOMObject
  def infix_getElementById(s: Rep[String])
}





trait ConditionalProg { this: Arith with Equal with Print with IfThenElse =>

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

trait DynamicProg { this: DynamicBase =>
  def test(x: Rep[Any]): Rep[Any] = {
    dynamic(x).foo_field.foo_fun().bar(x)
  }
}

trait LiteralProg { this: Arith with JSLiteral =>
  def test(x: Rep[Double]): Rep[Double] = {
    val o = new JSLiteral {
      val a = x
      val b = x + 2.0
      val c = 1.0
      val d = a + b
      val e = c + 2.0
    }
    o.a
  }

  def test2(x: Rep[Double]): Rep[Double] = {
    x + 2.0
  }

  def test3(x: Rep[Double]): Rep[Double] = {
    val o = new JSLiteral { val a = x + 2 }
    o.a
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
    lazy val id: Rep[Any => Any] = fun { x : Rep[Any] => id(x) }
    id(x)
  }
}
  
object Main extends App {
  println("-- begin")

  new ConditionalProg with ArithExpOpt with EqualExp with PrintExp
  with IfThenElseExp with CompileScala { self =>
    val codegen = new ScalaGenIfThenElse with ScalaGenArith 
    with ScalaGenEqual with ScalaGenPrint { val IR: self.type = self }
        
    val f = (x: Rep[Double]) => test(x)
    codegen.emitSource(f, "Test", new PrintWriter(System.out))
    val g = compile(f)
    println(g(7))
  }
    
  new ConditionalProg with IfThenElseExp with ArithExpOpt with EqualExp
  with PrintExp { self =>
    val codegen = new JSGenIfThenElse with JSGenArith 
    with JSGenEqual with JSGenPrint { val IR: self.type = self }
        
    val f = (x: Rep[Double]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
//    codegen.emitHTMLPage(() => f(7), new PrintWriter(System.out))
    
    val f2 = (x: Rep[Double]) => test2(x)
    codegen.emitSource(f2, "main", new PrintWriter(System.out))
  }
 
  new DynamicProg with DynamicExp { self =>
    val codegen = new JSGenDynamic { val IR: self.type = self }
    val f = (x: Rep[Any]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
  }

  println("-- end")
  
  new LiteralProg with JSLiteralExp with ArithExpOpt { self =>
    val codegen = new JSGenLiteral with JSGenArith { val IR: self.type = self }
    val f = (x: Rep[Double]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
  }

  new FunProg with JSFunctionsExp { self =>
    val codegen = new JSGenFunctions { val IR: self.type = self }
    val f = (x: Rep[Any]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
  }

  new SomeProg with JSExp { self =>
    val codegen = new JSGen { val IR: self.type = self }
    val f = (x: Rep[Any]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
  }
}
