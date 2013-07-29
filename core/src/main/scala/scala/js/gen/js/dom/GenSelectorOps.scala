package scala.js.gen.js.dom

import scala.js.exp.dom.SelectorOpsExp
import scala.js.gen.js.GenFFI

trait GenSelectorOps extends GenFFI { this: GenElementOps =>
  val IR: SelectorOpsExp
}