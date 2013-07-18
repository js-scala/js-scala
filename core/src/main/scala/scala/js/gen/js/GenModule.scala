package scala.js.gen.js

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericCodegen
import scala.js.gen.BaseGenModule

trait GenModule extends BaseGenModule with Codegen {
  def emitModule(name: String, out: java.io.PrintWriter) {
    withStream(out) {
      stream.println(s"""var $name = {""")
      emitModule(module)
      stream.println("};")
    }
  }
  protected def emitElement(m: Element) {
    m match {
      case f @ Function(arg, body) => emitSource(arg, body, "", stream)(f.Manifest)
      case m: Module =>
        stream.println("{")
        emitModule(m)
        stream.print("}")
    }
  }
  protected def emitModule(mod: Module) {
    mod.nes match {
      case (name, el) +: nes =>
        def emitEl(n: String, e: Element) = {
          stream.print(s""""$n": """)
          emitElement(e)
        }
        emitEl(name, el)
        for ((name, el) <- nes) {
          stream.println(",")
          emitEl(name, el)
        }
      case _ =>
    }
  }
}