package scala.js.gen.js

import scala.js.exp.JSExp

trait GenJS extends GenJsScala with GenDynamics with GenArrays
  with GenRegExps with GenOptionOps {
  val IR: JSExp
}

trait GenJSOpt extends GenJS with CodegenOpt