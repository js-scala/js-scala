package scala.js.language

/**
 * Extends the JsScala language with some client-side only DSLs
 */
trait JSBase extends JsScalaBase with Dynamics with Arrays with RegExps with OptionOps

/**
 * Same as [[scala.js.language.JSBase]] but with implicit conversions automatically lifting values to `Rep` values when needed.
 */
trait JS extends JSBase with JsScala