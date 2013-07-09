package scala.js.exp

import scala.js.language.Ajax
import scala.util.continuations._

trait AjaxExp extends Ajax with JSExp {
  
  case class AjaxGet(request: Rep[Request], success: Rep[Response => Unit]) extends Def[Unit]
  
  def ajax_get(request: Rep[Request]): Rep[Response] @suspendable = shift { k =>
    reflectEffect(AjaxGet(request, fun(k)))
  }

}