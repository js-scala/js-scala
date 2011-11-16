package scala.js.actor

import java.util.concurrent.CopyOnWriteArraySet

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocketServlet

import scala.collection.JavaConversions._

class WebSocketChatServlet extends WebSocketServlet {
  private var _count = 0;
  private val _members = new CopyOnWriteArraySet[ChatWebSocket]();

  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    getServletContext().getNamedDispatcher("default").forward(request, response)
  }

  override def doWebSocketConnect(request: HttpServletRequest, protocol: String) : WebSocket = new ChatWebSocket()

  class ChatWebSocket extends OnTextMessage {
    private var _connection : Connection = null;
    private var _id: Int = 0;

    def onOpen(connection: Connection) {
      // Log.info(this+" onConnect");
      _connection=connection;
      _count += 1;
      _id = _count;
      _members.add(this);
    }

    def onMessage(frame: Byte, data: Array[Byte], offset: Int, length: Int) {
      // Log.info(this+" onMessage: "+TypeUtil.toHexString(data,offset,length));
    }

    private def broadcast(data: String) {
      val xdata = data.replace("{", "{\"id\":" + _id + ",")
      for (member <- _members) {
        member._connection.sendMessage(xdata);
      }
    }

    def onMessage(data: String) {
      broadcast(data)
    }

    def onClose(code: Int, message: String) {
      // Log.info(this+" onDisconnect");
      _members.remove(this);
      broadcast("{\"action\":\"close\"}")
    }
  }
}
