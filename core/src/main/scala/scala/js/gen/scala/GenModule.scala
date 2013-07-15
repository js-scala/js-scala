package scala.js.gen.scala

import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase

  trait GenModule extends ScalaGenBase {
    def module: Module
    abstract sealed class Element
    case class Module(nes: (String, Element)*) extends Element
    case class Function[A : Manifest](args: List[IR.Sym[_]], body: Block[A]) extends Element {
      val Manifest = manifest[A]
    }
    def fun[T : Manifest, R : Manifest](f: (IR.Exp[T] => IR.Exp[R])): Function[R] = {
      val s = IR.fresh[T]
      val body = reifyBlock(f(s))
      val args = List(s)
      Function(args, body)
    }
    def fun[T1 : Manifest, T2 : Manifest, R : Manifest](f: ((IR.Exp[T1], IR.Exp[T2]) => IR.Exp[R])): Function[R] = {
      val s1 = IR.fresh[T1]
      val s2 = IR.fresh[T2]
      val body = reifyBlock(f(s1, s2))
      val args = List(s1, s2)
      Function(args, body)
    }
    def fun[T1 : Manifest, T2 : Manifest, T3 : Manifest, R : Manifest](f: ((IR.Exp[T1], IR.Exp[T2], IR.Exp[T3]) => IR.Exp[R])): Function[R] = {
      val s1 = IR.fresh[T1]
      val s2 = IR.fresh[T2]
      val s3 = IR.fresh[T3]
      val body = reifyBlock(f(s1, s2, s3))
      val args = List(s1, s2, s3)
      Function(args, body)
    }
    def emitModule(name: String, out: java.io.PrintWriter) {
      out.println(s"""object $name {""")
      emitModule(module, out)
      out.println("};")
      out.close()
    }
    protected def emitElement(m: Element, out: java.io.PrintWriter) {
      m match {
        case f @ Function(arg, body) => emitSource(arg, body,"" ,out)(f.Manifest)
        case m: Module => emitModule(m, out)
      }
    }
    protected def emitModule(mod: Module, out: java.io.PrintWriter) {
      mod.nes match {
        case (name, el) +: nes => 
          def emitEl(n: String, e: Element) = {
            out.print(s"""object $n {""")
            emitElement(e, out)
          }
          emitEl(name, el)
          for ((name, el) <- nes) {
            out.println("},")
            emitEl(name, el)
          }
          out.println("}")
        case _ =>
      }
    }
  }