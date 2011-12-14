package scala.js

import scala.virtualization.lms.common._
import scala.util.continuations._

import java.io.PrintWriter

import Predef.{any2stringadd => _, _}

trait TwitterApi extends JS with JSLib with CPS with Ajax with JSDebug {
  def append(loc: Rep[String], html: Rep[String]): Rep[Unit]
}

trait TwitterApiExp extends TwitterApi with JSExp with JSLibExp with CPSExp with AjaxExp with JSDebugExp {
  case class Append(loc: Exp[String], html: Exp[String]) extends Def[Unit]
  override def append(loc: Exp[String], html: Exp[String]): Exp[Unit] =
    reflectEffect(Append(loc, html))
}

trait JSGenTwitterApi extends JSGen with JSGenLib with GenCPS with GenAjax with JSGenDebug {
  val IR: TwitterApiExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Append(loc, html) => emitValDef(sym,
      "$(" + quote(loc) + ").append(" + quote(html) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

object Twitter {
  trait TwitterProg { this: TwitterApi with Casts =>
    def loadTweets(): Rep[Unit] = reset {
      for (user <- array("gkossakowski", "odersky", "adriaanm").parSuspendable) {
        log("fetching " + user)
        append("#jstwitter", string_plus("<div class='span-one-third'><h4>@", string_plus(user, string_plus("</h4><ul id='", string_plus(user, "'></ul></div>")))))
        val data = ajax.get {
          new JSLiteral {
            val url = "http://api.twitter.com/1/statuses/user_timeline.json"
            val `type` = "GET"
            val dataType = "jsonp"
            val data = new JSLiteral {
              val screen_name = user
              val include_rts = true
              val count = 5
              val include_entities = true
            }
          }
        }
        val tweets = data.as[Array[JSLiteral {val text: String}]]
        log("done fetching " + user)
        for (t <- tweets) {
          append("#" + user, "<li>" + t.text + "</li>")
        }
      }
      log("All done.")
    }
  }

  def codegen(pw: PrintWriter) {
    new TwitterProg with TwitterApiExp with Casts { self =>
      val codegen = new JSGenTwitterApi { val IR: self.type = self }
      codegen.emitSource0(loadTweets _, "loadTweets", pw)
    }
  }

 def writeJs(filename: String) = {
   val out = new PrintWriter(filename)
   codegen(out)
   out.close()
 }
}
