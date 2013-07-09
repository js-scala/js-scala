package scala.js.gen.js.dom

import scala.js.exp.dom.DomExp

trait GenDom extends GenCore with GenElementOps with GenEventOps with GenNodeListOps with GenBrowser with GenSelectorOps {
  val IR: DomExp
}
