package scala.js.language

/**
 * Extends the JsScala language with some client-side only DSLs
 */
trait JSBase extends JsScalaBase with Dynamics with Arrays with RegExps with OptionOps

trait JS extends JSBase with JsScala