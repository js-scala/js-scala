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

trait FunProg { this: JSFunctions =>
  def test(x: Rep[Any]): Rep[Any] = {
    val id = fun { x : Rep[Any] => x }
    id.apply(x)
  }
}

trait SomeProg { this: JS =>
  def test(x: Rep[Any]): Rep[Any] = {
    lazy val id: Rep[Any => Any] = fun { x : Rep[Any] =>
      dynamic(x).foo.bar()
      id.apply(x)
    }
    id.apply(x)
  }
}

object ProxyDog {
  // adapated from http://stackoverflow.com/questions/3291637/alternatives-to-java-lang-reflect-proxy-for-creating-proxies-of-abstract-classes

  import javassist._
  import javassist.util.proxy._
  import java.lang.{reflect => jreflect}

  abstract class Dog {
    def bark(): Unit = println("Woof!")
    def fetch(): Unit
  }

  val factory = new ProxyFactory()
  factory.setSuperclass(classOf[Dog])
  factory.setFilter(
    new MethodFilter() {
      override def isHandled(method: jreflect.Method) =
        Modifier.isAbstract(method.getModifiers())
    })
  val handler = new MethodHandler() {
    override def invoke(self: AnyRef, thisMethod: jreflect.Method, proceed: jreflect.Method, args: Array[AnyRef]): AnyRef = {
      println("Handling " + thisMethod + " via the method handler")
      null
    }
  }
  val dog = factory.create(Array[Class[_]](), Array[AnyRef](), handler).asInstanceOf[Dog]

  def run() = {
    dog.bark()
    dog.fetch()
  }
}

object Main extends App {
  new FunProg with JSFunctionsExp { self =>
    val codegen = new JSGenFunctions { val IR: self.type = self }
    codegen.emitSource(test _, "main", new PrintWriter(System.out))
  }

  new SomeProg with JSExp { self =>
    val codegen = new JSGen { val IR: self.type = self }
    codegen.emitSource(test _, "main", new PrintWriter(System.out))
  }

  Koch.writeHtml("koch.html")
  if (args.contains("graphics"))
    Koch.run()
  Birds.writeJs("examples/birds/Bird_.js")
  Twitter.writeJs("examples/ajax/twitter_.js")

  ProxyDog.run()
}
