package scala.js.exp

import scala.js.language.JS
import scala.js.exp.dom.DomExpOpt

trait JSExp extends JS with JsScalaExp with DynamicsExp with ArraysExp
  with RegExpsExp with OptionOpsExp

trait JSExpOpt extends JSExp with JsScalaExpOpt with DomExpOpt