package scala.js.exp.dom

import scala.js.language.dom.SelectorOps
import scala.virtualization.lms.common.EffectExp
import scala.js.exp.FFIExp

trait SelectorOpsExp extends SelectorOps with EffectExp with NodeListOpsExp with FFIExp { this: ElementOpsExp =>

  def selector_find[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) =
    foreign"$s.querySelector($selector)"[Option[A]].withEffect()

  def selector_findAll[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) =
    foreign"$s.querySelectorAll($selector)"[NodeList[A]].withEffect()

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
          case Id(id, _) if s == document => foreign"$s.getElementById(${unit(id)})"[Option[A]].withEffect()
          case _ => super.selector_find(s, selector)
        }
      case _ => super.selector_find(s, selector)
    }

  }

  override def selector_findAll[A : Selectable : Manifest](s: Exp[Selector], selector: Exp[String]) = {
    //the Regex to recover the class
    val ClassName = "\\.((-?[A-Za-z0-9_]+)+)".r
    //the Regex to recover the tag
    val Tag = "((-?[A-Za-z0-9_]+)+)".r

    selector match {
      //if it is a constant
      case Const(selectorString) =>
        selectorString.trim match {
          //if the first charactere is a '.' we are searching a class
          case ClassName(name, _) => foreign"$s.getElementsByClassName(${unit(name)})"[NodeList[A]].withEffect()
          //if it's a character chain we are searching a tag
          case Tag(tag, _) => foreign"$s.getElementsByTagName(${unit(tag)})"[NodeList[A]].withEffect()
          case _ => super.selector_findAll(s, selector)
        }
      case _ => super.selector_findAll(s, selector)
    }
  }

}