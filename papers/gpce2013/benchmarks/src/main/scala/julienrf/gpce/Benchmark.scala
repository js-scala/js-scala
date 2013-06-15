package julienrf.gpce

import scala.js.language.{OptionOps, dom}
import scala.virtualization.lms.common.{LiftString, NumericOps, Base, TupledFunctions}

trait Benchmark extends Base with OptionOps with TupledFunctions with NumericOps with dom.Dom with LiftString {

  lazy val maybe = fun { (x: Rep[Int]) =>
    some(x + unit(1))
  }

  def benchmark = for {
    a <- maybe(unit(0))
    b <- maybe(a)
    c <- maybe(b)
    d <- maybe(c)
  } yield d

  def getWords() = {
    for (form <- document.find("#add-user")) yield {
       val sections = form.findAll("fieldset")
      sections map (_.findAll(".word"))
    }
  }
}
