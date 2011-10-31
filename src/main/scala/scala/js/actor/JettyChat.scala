package scala.js.actor

import org.eclipse.jetty.websocket.WebSocket.Connection
import java.util.concurrent.CopyOnWriteArraySet
import scala.collection.JavaConversions._

class ChatWebSocket extends OnTextMessage {

  val _members = new CopyOnWriteArraySet[ChatWebSocket]();

  var _connection : Connection = null;

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
