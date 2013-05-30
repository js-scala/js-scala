package scala.js.language

trait Traits extends Proxy {
  trait Factory[+T] {
    def apply(): Rep[T]
  }
  def register[T<:AnyRef:Manifest](outer: AnyRef): Factory[T]
}