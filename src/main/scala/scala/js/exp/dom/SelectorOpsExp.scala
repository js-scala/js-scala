package scala.js.exp.dom

import scala.js.language.dom.SelectorOps
import scala.virtualization.lms.common.EffectExp

trait SelectorOpsExp extends SelectorOps with EffectExp with ElementOpsExp {

  def selector_find[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) =
    reflectEffect(SelectorFind[A](s, selector))

  def selector_findAll[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) =
     reflectEffect(SelectorFindAll[A](s, selector))

  case class SelectorFind[A : Selectable](s: Exp[Selector], selector: Exp[String]) extends Def[Option[A]]
  case class SelectorGetElementById[A : Selectable](s: Exp[Selector], selector: Exp[String]) extends Def[Option[A]]
  case class SelectorFindAll[A : Selectable](s: Exp[Selector], selector: Exp[String]) extends Def[NodeList[A]]
  case class SelectorGetElementsByClassName[A : Selectable](s: Exp[Selector], selector: Exp[String]) extends Def[NodeList[A]]
  case class SelectorGetElementsByTagName[A : Selectable](s: Exp[Selector], selector: Exp[String]) extends Def[NodeList[A]]

}

trait SelectorOpsExpOpt extends SelectorOpsExp with BrowserExp {

  override def selector_find[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) = {
    //the Regex to recover the ID
    val Id = "#((-?[A-Za-z0-9_]+)+)".r

    selector match {
      //if it is a constant
      case Const(selectorString) =>
        selectorString.trim match {
          //if the first charactere is a '#' we are searching a ID
          case Id(id, _) if s == document => reflectEffect(SelectorGetElementById[A](s, unit(id)))
          case _ => super.selector_find(s, selector)
        }
      case _ => super.selector_find(s, selector)
    }

  }

  override def selector_findAll[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) = {
    //the Regex to recover the class
    val Classe = "\\.((-?[A-Za-z0-9_]+)+)".r
    //the Regex to recover the tag
    val Tag = "((-?[A-Za-z0-9_]+)+)".r

    selector match {
      //if it is a constant
      case Const(selectorString) =>
        selectorString.trim match {
          //if the first charactere is a '.' we are searching a class
          case Classe(classe, _) => reflectEffect(SelectorGetElementsByClassName[A](s, unit(classe)))
          //if it's a character chain we are searching a tag
          case Tag(tag, _) => reflectEffect(SelectorGetElementsByTagName[A](s, unit(tag)))
          case _ => super.selector_findAll(s, selector)
        }
      case _ => super.selector_findAll(s, selector)
    }
  }

}