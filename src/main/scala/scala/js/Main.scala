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

object ProxyFoo {
  import javassist._

  class Foo(v: Int) {
    private var w = 0
    def f() = v
    def g() = v+w
    def h() = { w += 1; w }
  }

  def run(): Unit = {
    val cp = ClassPool.getDefault()
    cp.insertClassPath(new ClassClassPath(classOf[Foo]))
    val cl = new Loader(cp)

    val cc = cp.get(classOf[Foo].getName())

    // We only instrument methods and not constructors, because fields
    // can be set before the object is initialized to an Object. We
    // can copy the constructor into a normal method to instrument it.
    val conv = new CodeConverter()

    val ch = cp.makeClass(classOf[Foo].getName() + "__$StaticHelper")

    for (cf <- cc.getDeclaredFields()) {
      cf.setModifiers(Modifier.PUBLIC)

      ch.addMethod(CtNewMethod.make("public static " + cf.getType().getName() + " __" + cf.getName() + "(Object target) { System.out.println(\"** field-read: " + cf.getName() + "\"); return ((" + cc.getName() + ") target)." + cf.getName() +"; }", ch))
      conv.replaceFieldRead(cf, ch, "__" + cf.getName())

      ch.addMethod(CtNewMethod.make("public static void __" + cf.getName() + "_$eq(Object target, " + cf.getType().getName() + " value) { System.out.println(\"** field-write: " + cf.getName() + "\"); ((" + cc.getName() + ") target)." + cf.getName() + " = value; }", ch))
      conv.replaceFieldWrite(cf, ch, "__" + cf.getName() + "_$eq")
    }

    for (cm <- cc.getDeclaredConstructors()) {
      cc.addMethod(cm.toMethod("constructor", cc))
      cm.insertBefore("System.out.println(\"** constructor-call: \" + \"" + cm.getName() + "\");")
    }
    for (cm <- cc.getDeclaredMethods()) {
      cm.insertBefore("System.out.println(\"** method-call: \" + \"" + cm.getName() + "\");")
      cm.instrument(conv)
    }
    cc.writeFile()

    val fooClazz = cl.loadClass(classOf[Foo].getName())
    val fooConstructor = fooClazz.getConstructor(classOf[Int])
    val foo = fooConstructor.newInstance(2: java.lang.Integer)

    fooClazz.getDeclaredMethod("f").invoke(foo)
    fooClazz.getDeclaredMethod("g").invoke(foo)
    fooClazz.getDeclaredMethod("h").invoke(foo)

    fooClazz.getDeclaredMethod("constructor", classOf[Int]).invoke(foo, 2: java.lang.Integer)
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
  ProxyFoo.run()
}
