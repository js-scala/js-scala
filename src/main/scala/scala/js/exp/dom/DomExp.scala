package scala.js.exp.dom

import scala.js.language.dom.Dom

trait DomExp extends Dom with CoreExp with ElementOpsExp with EventOpsExp with NodeListOpsExp with BrowserExp with SelectorOpsExp
