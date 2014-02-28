package scala.js.language

import scala.language.experimental.macros
import scala.virtualization.lms.common.Functions
import scala.annotation.StaticAnnotation

/**
 * Turns case class hierarchies into staged data types with support for smart constructors,
 * members selection, structural comparison, copy method “a la” case classes, and fold over sum types.
 * 
 * Example of a record type (a product type with labelled members):
 * 
 * {{{
 *   // --- Definition
 *   @adt case class Point(x: Int, y: Int) // regular case class annotated with `@adt`
 *
 *   // --- Usage
 *   def add(p1: Rep[Point], p2: Rep[Point]): Rep[Point] =
 *     Point(p1.x + p2.x, p1.y + p2.y) // smart constructor and members selection
 *
 *   def check(p1: Rep[Point], p2: Rep[Point]): Rep[Boolean] =
 *     p1 === p2 // structural equality (`==` can not be overridden on Rep values)
 *
 *   def verticalProjection(p: Rep[Point]): Rep[Point] =
 *     p.copy(x = 0) // similar to case classes `copy` method
 * }}}
 *
 * Another example with a sum type:
 *
 * {{{
 *   // --- Definition
 *   @adt sealed trait CoProduct
 *   @adt case class Left(x: Int) extends CoProduct // the annotation *must* be repeated on each variant of `CoProduct`
 *   @adt case class Right(s: String) extends CoProduct
 *
 *   // --- Usage
 *   def foo(c: Rep[CoProduct]) = c.fold( // poor man’s pattern matching (see SI-7077)
 *     (l: Rep[Left]) => "left",
 *     (r: Rep[Right]) => "right"
 *   )
 *
 *   def check(c1: Rep[CoProduct], c2: Rep[CoProduct]): Rep[Boolean] = {
 *     c1 === c2 // you can compare Rep[Left] and Rep[Right] values with Rep[CoProduct] values but you can not compare Rep[Right] values with Rep[Left] values
 *   }
 * }}}
 */
trait Adts extends Functions {

  def adt_construct[A : Manifest](fields: (String, Rep[_])*): Rep[A]
  def adt_select[A : Manifest, B : Manifest](obj: Rep[A], label: String): Rep[B]
  def adt_equal[A : Manifest](a1: Rep[A], a2: Rep[A], fields: Seq[Rep[Boolean]]): Rep[Boolean]
  def adt_field_equal[A](a1: Rep[A], a2: Rep[A], field: String): Rep[Boolean] // FIXME Compare several fields at once?
  def adt_fold[R : Manifest, A : Manifest](obj: Rep[R], fs: Seq[Rep[_ <: R => A]]): Rep[A]

}

object Adts {

  import scala.reflect.macros.Context

  /**
   * On a case class Foo(bar: String, baz: Int), expands to the following companion object:
   *
   * {{{
   *   object Foo {
   *     // smart constructor
   *     def apply(bar: Rep[String], baz: Rep[Int]): Rep[Foo] = ???
   *     // pimped ops
   *     implicit class FooOps(self: Rep[Foo]) {
   *       // members selection
   *       def bar: Rep[String] = ???
   *       def baz: Rep[Int] = ???
   *       // copy
   *       def copy(bar: Rep[String] = self.bar, baz: Rep[Int] = self.baz): Rep[Foo] = Foo.apply(bar, baz)
   *       // structural equality
   *       def === (that: Rep[Foo]): Rep[Boolean] = ???
   *     }
   *   }
   * }}}
   */
  class adt extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro impl
  }

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val Rep = tq"Rep" // TODO Take it as a parameter (see https://github.com/scalamacros/paradise/issues/2 and https://github.com/scalamacros/paradise/issues/8)
    // Annottees must be sealed traits, case classes or a case objects
    val outputs = annottees.head.tree match {
      case t @ q"sealed trait $name" =>
        List(t)
      case clazz @ q"case class $className (..$members)" =>

        // Smart constructor
        val liftedArgs = for (q"$mods val $name: $tpe = $rhs" <- members) yield q"val $name: $Rep[$tpe] = $rhs"
        val effectiveArgs = for (q"$mods val $name: $tpe = $rhs" <- members) yield q"(${name.decoded}, $name)"
        val constructor = q"""def apply(..$liftedArgs): $Rep[$className] = adt_construct[$className](..$effectiveArgs)"""

        val self = newTermName(c.fresh())

        // Members selection
        val membersSelection =
          for (q"$mods val $name: $tpe = $rhs" <- members)
          yield q"def $name: $Rep[$tpe] = adt_select[$className, $tpe]($self, ${name.decoded})"

        // Copy
        val copyArgs =
          for (q"$mods val $name: $tpe = $rhs" <- liftedArgs)
          yield q"val $name: $tpe = $self.$name"
        val copyEffectiveArgs = for (q"$mods val $name: $tpe = $rhs" <- liftedArgs) yield q"$name"
        val copy = q"def copy(..$copyArgs): $Rep[$className] = ${className.toTermName}.apply(..$copyEffectiveArgs)"

        // Equals
        val that = newTermName(c.fresh())
        val memberNames = for (q"$mods val $name: $tpe = $rhs" <- members) yield {
          // FIXME `tpe.symbol` is always `NoSymbol`
          if (tpe.symbol.annotations.exists(_.tpe =:= typeOf[adt])) q"$self.$name === $that.$name"
          else q"adt_field_equal($self, $that, ${name.decoded})"
        }
        val equal = q"""
          def === ($that: $Rep[$className]): $Rep[Boolean] =
            adt_equal($self, $that, Seq(..$memberNames))
        """

        // Ops
        val ops = q"""
          implicit class ${newTypeName(c.fresh())}($self: $Rep[$className]) {
            ..$membersSelection
            $copy
            $equal
          }
        """

        // Companion
        val companion = q"""object ${className.toTermName} {
          $constructor
          $ops
        }"""

        List(clazz, companion)
      case o @ q"case object $name" =>
        List(o)
      case _ => c.abort(c.enclosingPosition, "The @adt annotation must be used only on sealed traits, case classes and case objects")
    }

    // Expansion of an annotation must be a block returning Unit and containing the sequence of all the annottees expansions
    c.Expr[Any](q"{ ..$outputs; () }")
  }

}
