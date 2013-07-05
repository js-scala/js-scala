package scala.js.gen.js

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericCodegen


  trait GenModule extends Codegen {
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
    def emitModule(out: java.io.PrintWriter) {
      out.println("define([],function(){")
      out.println("return{")
      emitModule(module, out)
      out.println("}")
      out.println("});")
      out.close()
    }
    protected def emitElement(m: Element, out: java.io.PrintWriter) {
      m match {
        case f @ Function(arg, body) => emitSource(arg, body,"",out)(f.Manifest)
        case m: Module =>
          out.println("{")
          emitModule(m, out)
          out.print("}")
      }
    }
    protected def emitModule(mod: Module, out: java.io.PrintWriter) {
      mod.nes match {
        case (name, el) +: nes => 
          def emitEl(n: String, e: Element) = {
            out.print(s""""$n": """)
            emitElement(e, out)
          }
          emitEl(name, el)
          for ((name, el) <- nes) {
            out.println(",")
            emitEl(name, el)
          }
        case _ =>
      }
    }
  }