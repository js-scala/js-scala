package scala.js.gen.js

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericCodegen
import scala.js.gen.BaseGenModule

trait GenModuleAMD extends BaseGenModule with Codegen{
  def emitModule(out: java.io.PrintWriter) {
    out.println("define([],function(){")
    out.println("return{")
    emitModule(module, out)
    out.println("}")
    out.println("});")
    out.flush()
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