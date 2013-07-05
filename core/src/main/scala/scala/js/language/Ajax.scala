package scala.js.language

trait Ajax extends JS with CPS {

  type Request = Record {
    val url: String
    val `type`: String
    val dataType: String
    val data: Record
  }

  type Response = Any

  val ajax = new AjaxOps
  
  class AjaxOps {
    def get(request: Rep[Request]): Rep[Response] @suspendable =
      ajax_get(request)
  }

  def ajax_get(request: Rep[Request]): Rep[Response] @suspendable

}