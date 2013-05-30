package scala.js.exp

import scala.js.language.JS

trait JSExp extends JS with JsScalaExp with DynamicsExp with ArraysExp
  with RegExpsExp with OptionOpsExp

trait JSExpOpt extends JSExp with JsScalaExpOpt