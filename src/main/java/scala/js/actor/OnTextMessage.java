package scala.js.actor;

/** Dummy Java interface that serves as a work-around for illegal cyclic
    dependency problem in Scala compiler. */
interface OnTextMessage extends org.eclipse.jetty.websocket.WebSocket.OnTextMessage {

}
