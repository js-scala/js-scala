import scala.virtualization.lms.common._

import java.io.PrintWriter
import java.io.FileOutputStream


trait JSGenEqual extends JSGenBase {
  val IR: EqualExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, "" + quote(a) + "==" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}



trait Print extends Base {
  implicit def unit(s: String): Rep[String]
  def print(s: Rep[Any]): Rep[Unit]
}

trait PrintExp extends Print with EffectExp {
  implicit def unit(s: String): Rep[String] = Const(s)
  case class Print(s: Rep[Any]) extends Def[Unit]
  def print(s: Rep[Any]) = reflectEffect(Print(s))
}

trait DynamicBase extends Base {
  protected type DynamicRep <: DynamicRepImpl with Rep[Any] with Dynamic
  protected trait DynamicRepImpl extends Dynamic {
    def applyDynamic(method: String)(args: Rep[Any]*): DynamicRep
  }
  protected def dynamic(x: Rep[Any]): DynamicRep
}

trait DynamicExp extends DynamicBase with EffectExp {
  
  type DynamicRep = DynamicExp

  case class DynamicCall(receiver: Rep[Any], method: String, args: List[Rep[Any]]) extends Def[Any]
  
  case class DynamicExp(receiver: Rep[Any]) extends Rep[Any] with DynamicRepImpl {
    override def applyDynamic(method: String)(args: Rep[Any]*): DynamicRep =
      DynamicExp(reflectEffect(DynamicCall(receiver, method, args.toList)))
  }
  
  def dynamic(x: Rep[Any]) = DynamicExp(x)

}

trait JSGenDynamicCall extends JSGenEffect {
  val IR: DynamicExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DynamicCall(receiver, method, args) =>  emitValDef(sym, 
      quote(receiver) + "." + method + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym, rhs)
  }
  
  override def quote(x: Exp[Any]) : String = x match {
    case DynamicExp(receiver) => quote(receiver)
    case _ => super.quote(x)
  }
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
    dynamic(x).foo.bar(x)
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
    codegen.emitHTMLPage(() => f(7), new PrintWriter(System.out))
  }
 
  new DynamicProg with DynamicExp { self =>
    val codegen = new JSGenDynamicCall { val IR: self.type = self }
    val f = (x: Rep[Any]) => test(x)
    codegen.emitSource(f, "main", new PrintWriter(System.out))
  }

  println("-- end")
}
