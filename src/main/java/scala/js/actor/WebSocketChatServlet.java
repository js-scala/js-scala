package scala.js.actor;

import java.io.IOException;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.eclipse.jetty.util.TypeUtil;
import org.eclipse.jetty.util.log.Log;
import org.eclipse.jetty.websocket.WebSocket;
import org.eclipse.jetty.websocket.WebSocketServlet;

public class WebSocketChatServlet extends WebSocketServlet
{
    private final Set<ChatWebSocket> _members = new CopyOnWriteArraySet<ChatWebSocket>();

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
        throws javax.servlet.ServletException ,IOException
    {
        getServletContext().getNamedDispatcher("default").forward(request,response);
    };

    public WebSocket doWebSocketConnect(HttpServletRequest request, String protocol)
    {
        return new ChatWebSocket();
    }

}
