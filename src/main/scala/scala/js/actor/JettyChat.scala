package scala.js.actor

import java.util.concurrent.CopyOnWriteArraySet

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.eclipse.jetty.websocket.WebSocket
import org.eclipse.jetty.websocket.WebSocket.Connection
import org.eclipse.jetty.websocket.WebSocketServlet

import scala.collection.JavaConversions._

class WebSocketChatServlet extends WebSocketServlet {
  private val _members = new CopyOnWriteArraySet[ChatWebSocket]();

  override protected def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    getServletContext().getNamedDispatcher("default").forward(request, response)
  }

  override def doWebSocketConnect(request: HttpServletRequest, protocol: String) : WebSocket = new ChatWebSocket()

  class ChatWebSocket extends OnTextMessage {
    private var _connection : Connection = null;

    def onOpen(connection: Connection) {
      // Log.info(this+" onConnect");
      _connection=connection;
      _members.add(this);
    }

    def onMessage(frame: Byte, data: Array[Byte], offset: Int, length: Int) {
      // Log.info(this+" onMessage: "+TypeUtil.toHexString(data,offset,length));
    }

    def onMessage(data: String) {
      if (data.indexOf("disconnect")>=0)
        _connection.disconnect();
      else {
        // Log.info(this+" onMessage: "+data);
        for (member <- _members) {
          member._connection.sendMessage(data);
        }
      }
    }

    def onClose(code: Int, message: String) {
      // Log.info(this+" onDisconnect");
      _members.remove(this);
    }
  }
}
