package julienrf.gpce

import scala.js.exp.OptionOpsExp
import scala.js.exp.dom.DomExpOpt
import scala.js.gen.js.{GenOptionOps, GenFunctions, GenNumericOps}
import scala.js.gen.js.dom.GenDom
import scala.virtualization.lms.common._

object Main extends App {

  val benchmark = new Benchmark with OptionOpsExp with TupledFunctionsExp with NumericOpsExp with DomExpOpt
  val gen = new GenOptionOps with GenFunctions with GenericGenUnboxedTupleAccess with GenNumericOps with GenDom { val IR: benchmark.type = benchmark }
  gen.emitSource0(() => benchmark.benchmark, "jsscala", new java.io.PrintWriter("js-scala.js"))
  gen.emitSource0(() => benchmark.getWords(), "selectorsJsScala", new java.io.PrintWriter("selectors-js-scala.js"))

}
