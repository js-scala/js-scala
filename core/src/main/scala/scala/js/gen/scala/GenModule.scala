package scala.js.gen.scala

import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.js.gen.BaseGenModule

trait GenModule extends BaseGenModule with ScalaGenBase {
  def emitModule(name: String, out: java.io.PrintWriter) {
    out.println(s"""object $name {""")
    emitModule(module, out)
    out.println("}")
    out.close()
  }
  protected def emitModule(mod: Module, out: java.io.PrintWriter) { 
    for ((n, e) <- mod.nes) {
      out.print(s"""object $n """)
      e match {
          case f @ Function(arg, body) =>
            out.print(s"""extends $n """)
            emitSource(arg, body,n ,out)(f.Manifest)
          case m: Module =>
            out.print("{")
            emitModule(m, out)
            out.println("}")
      }
    }
  }
}