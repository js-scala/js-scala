package scala.js

import scala.virtualization.lms.common._

import java.io.PrintWriter

trait MiceApi extends JSProxyBase with JSLiteral {
  val document: Rep[Any]
  val window: Rep[Any]

  type DataLiteral = JSLiteral {val data : String}
  def initWebSocket(): Rep[WebSocket]
  trait WebSocket {
    var onmessage: Rep[DataLiteral => Any]
    def send(data: Rep[String]): Rep[Unit]
  }
  implicit def repToSocket(x: Rep[WebSocket]): WebSocket = repProxy[WebSocket](x)

  type ActionLiteral = JSLiteral {val action: String; val id: String}
  type MoveLiteral = JSLiteral {val id: String; val cx: Int; val cy: Int; val w: Int; val h: Int}

  def currentTime(): Rep[Int]

  val json: Rep[JSON]
  trait JSON {
    def stringify(literal: Rep[JSLiteral]): Rep[String]
    def parse[T <: JSLiteral](data: Rep[String]): Rep[T]
  }
  implicit def repToJSON(x: Rep[JSON]): JSON = repProxy[JSON](x)

  def jQuery(x: Rep[Any]): Rep[JQuery]
  trait JQuery {
    var length: Rep[Int]

    def width(): Rep[Int]
    def height(): Rep[Int]

    def append(s: Rep[String]): Rep[JQuery]
    def css(o: Rep[JSLiteral]): Rep[JQuery]
    def mousemove(fn: Rep[JQueryEvent => Any]): Rep[JQuery]
    def remove(): Rep[JQuery]
  }
  implicit def repToJQuery(x: Rep[JQuery]): JQuery = repProxy[JQuery](x)
  type JQueryEvent = JSLiteral {val pageX: Int; val pageY: Int}
}

trait MiceApiExp extends MiceApi with JSProxyExp with JSLiteralExp {
  case object DocumentVar extends Exp[Any]
  val document = DocumentVar

  case object WindowVar extends Exp[Any]
  val window = WindowVar

  case object InitWebSocket extends Def[WebSocket]
  def initWebSocket() = reflectEffect(InitWebSocket)

  case object CurrentTime extends Def[Int]
  def currentTime() = reflectEffect(CurrentTime)

  case object JSONVar extends Exp[JSON]
  val json = JSONVar

  case class JQueryCall(x: Exp[Any]) extends Def[JQuery]
  def jQuery(x: Exp[Any]) = JQueryCall(x)
}

trait JSGenMiceApi extends JSGenProxy with JSGenLiteral {
  val IR: MiceApiExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case DocumentVar => "document"
    case WindowVar => "window"
    case JSONVar => "JSON"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case InitWebSocket => emitValDef(sym,
      "new WebSocket(document.location.toString().replace('http://','ws://').replace('https://','wss://'))")
    case CurrentTime => emitValDef(sym,
      "(new Date()).getTime()")
    case JQueryCall(x) => emitValDef(sym,
      "$(" + quote(x) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

object Mice {
  trait MiceProg { this: JS with MiceApi with LiftVariables =>
    def main() {
      val move = fun { (mouse: Rep[MoveLiteral]) =>
        if (jQuery("#mouse_"+mouse.id).length == 0)
          jQuery("body").append("<span class='mouse' id='mouse_"+mouse.id+"'><span style='display:none;' class='chat'/></span>")
        jQuery("#mouse_"+mouse.id).css(new JSLiteral {
          val left = ((jQuery(window).width().asInstanceOf[Rep[Int]] - mouse.w) / 2 + mouse.cx) + "px"
          val top = mouse.cy + "px"
        })
      }

      val ratelimit = fun { (fn: Rep[JQueryEvent => Any], ms: Rep[Int]) =>
        var last = currentTime()
        fun { (e: Rep[JQueryEvent]) =>
          val now = currentTime()
          if (now - last > ms) {
            last = now
            fn(e)
          }
        }
      }

      val socket = initWebSocket()
      socket.onmessage = fun { (m: Rep[DataLiteral]) =>
        val data = json.parse(m.data).asInstanceOf[Rep[ActionLiteral]]
        if (data.action == "close")
          jQuery("#mouse_"+data.id).remove()
        else if (data.action == "move")
          move(data.asInstanceOf[Rep[MoveLiteral]])
      }

      jQuery(document).mousemove(
        ratelimit((fun { (e: Rep[JQueryEvent]) =>
          socket.send(json.stringify(new JSLiteral {
            val action = "move"
            val cx = e.pageX
            val cy = e.pageY
            val w = jQuery(window).width()
            val h = jQuery(window).height()
          }))
        }), 40)
      )
    }
  }

  def codegen(pw: PrintWriter) {
    new MiceProg with JSExp with MiceApiExp with LiftVariables { self =>
      val codegen = new JSGenOpt with JSGenMiceApi { val IR: self.type = self }
      codegen.emitSource0(main _, "main", pw)
    }
  }

 def writeJs(filename: String) = {
   val out = new PrintWriter(filename)
   codegen(out)
   out.close()
 }
}
