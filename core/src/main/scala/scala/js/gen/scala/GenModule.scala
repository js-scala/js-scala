package scala.js.gen.scala

import java.io.PrintWriter
import scala.virtualization.lms.common.ScalaGenBase
import scala.js.gen.BaseGenModule

trait GenModule extends BaseGenModule with ScalaGenBase {
  def emitModule(name: String, out: java.io.PrintWriter) {
    withStream(out){
    stream.println(s"""object $name {""")
    emitModule(module)
    stream.println("}")}
  }
  protected def emitModule(mod: Module) { 
    for ((n, e) <- mod.nes) {
      stream.print(s"""object $n """)
      e match {
          case f @ Function(arg, body) =>
            stream.print(s"""extends $n """)
            emitSource(arg, body,n ,stream)(f.Manifest)
          case m: Module =>
            stream.print("{")
            emitModule(m)
            stream.println("}")
      }
    }
  }
}