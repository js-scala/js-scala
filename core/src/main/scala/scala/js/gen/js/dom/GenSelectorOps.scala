package scala.js.gen.js.dom

import scala.js.exp.dom.SelectorOpsExp
import scala.js.gen.js.{GenFFI, GenEffect}

trait GenSelectorOps extends GenElementOps with GenFFI {
  val IR: SelectorOpsExp
}