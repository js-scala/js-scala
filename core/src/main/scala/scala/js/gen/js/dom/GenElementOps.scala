package scala.js.gen.js.dom

import scala.js.exp.dom.ElementOpsExp
import scala.js.gen.js.GenEffect

trait GenElementOps extends GenEffect {
  val IR: ElementOpsExp
  import IR._

  
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ElementSetAttribute(e, name, value) =>
      emitValDef(sym, quote(e) + ".setAttribute(" + quote(name) + ", " + quote(value) + ")")
    case ElementTagName(e) =>
      emitValDef(sym, quote(e) + ".tagName")
    case ElementClassName(e) =>
      emitValDef(sym, quote(e) + ".className")
    case ElementParentNode(e) =>
      emitValDef(sym, quote(e) + ".parentNode")
    case ElementPreviousSibling(e) =>
      emitValDef(sym, quote(e) + ".previousSibling")
    case ElementClassList(e) =>
      emitValDef(sym, quote(e) + ".classList")
    case ElementRemoveChild(e, c) =>
      emitValDef(sym, quote(e) + ".removeChild(" + quote(c) + ")")
    case ElementAppendChild(e, c) =>
      emitValDef(sym, quote(e) + ".appendChild(" + quote(c) + ")")
    case ElementFocus(e) =>
      emitValDef(sym, quote(e) + ".focus()")
    case ElementStyle(e) =>
      emitValDef(sym, quote(e) + ".style")
      
    case InputSetDisabled(input, value) =>
      emitAssignment(quote(input) + ".disabled", quote(value))
    case InputDisabled(input) =>
      emitValDef(sym, quote(input) + ".disabled")
    case InputSetName(input, name) =>
      emitAssignment(s"${quote(input)}.name", quote(name))
    case InputName(input) =>
      emitValDef(sym, s"${quote(input)}.name")
    case InputValue(input) =>
      emitValDef(sym, s"${quote(input)}.value")
    case InputSetChecked(input, value) =>
      emitAssignment(quote(input) + ".checked", quote(value))
    case InputChecked(input) =>
      emitValDef(sym, quote(input) + ".checked")

    case FormSubmit(form) =>
      emitValDef(sym, quote(form) + ".submit()")
      
    case CSSFontWeight(css) =>
      emitValDef(sym, quote(css) + ".fontWeight")
    case CSSSetFontWeight(css, v) =>
      emitAssignment(s"${quote(css)}.fontWeight", quote(v))
    
    case _ => super.emitNode(sym, rhs)
  }
}