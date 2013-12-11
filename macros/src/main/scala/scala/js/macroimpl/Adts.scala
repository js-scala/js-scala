package scala.js.macroimpl

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Adts {
  
  trait Adt

  def adt[U <: Adt : c.WeakTypeTag](c: Context) =
    c.Expr[Any](new Generator[c.type](c).construct[U])


  def ops[U <: Adt : c.WeakTypeTag, R[_]](c: Context)(o: c.Expr[R[U]]) =
    c.Expr[Any](new Generator[c.type](c).ops(o))


  class Generator[C <: Context](val c: C) {
    import c.universe._

    /**
     * @return The whole class hierarchy the type `A` belongs to. Works only with closed class hierarchies.
     *         The symbols are sorted by alphabetic order.
     */
    def wholeHierarchy[A <: Adt : WeakTypeTag]: Seq[ClassSymbol] = {

      val rootClass: ClassSymbol =
        weakTypeOf[A].baseClasses
          // Take up to `Adt` super type
          .takeWhile(_.asClass.toType != typeOf[Adt])
          // Filter out type ancestors automatically added to case classes
          .filterNot { s =>
            val tpe = s.asClass.toType
            tpe =:= typeOf[Equals] || tpe =:= typeOf[Serializable] || tpe =:= typeOf[java.io.Serializable] || tpe =:= typeOf[Product]
          }.last.asClass // We know there is at least one element in the list because of `baseClasses`

      def subHierarchy(base: ClassSymbol): List[ClassSymbol] = {
        base.typeSignature // Needed before calling knownDirectSubclasses (SI-7046)
        base.knownDirectSubclasses.foldLeft(List(base)) { (result, symbol) =>
          val clazz = symbol.asClass
          if (clazz.isCaseClass) clazz :: result
          else if (clazz.isSealed && (clazz.isTrait || clazz.isAbstractClass)) subHierarchy(clazz) ++ result
          else c.abort(c.enclosingPosition, "A class hierarchy may only contain case classes, sealed traits and sealed abstract classes")
        }
      }

      subHierarchy(rootClass)
        .sortBy(_.name.decoded)
        .ensuring(_.nonEmpty, s"Oops: whole hierarchy of $rootClass is empty")
    }

    /**
     * @return The class hierarchy of the type `A`, meaning, `A` and all its subclasses
     */
    def hierarchy[A <: Adt : WeakTypeTag]: Seq[ClassSymbol] =
      wholeHierarchy[A]
        .filter(_.toType <:< weakTypeOf[A])
        .ensuring(_.nonEmpty, s"Oops: hierarchy of ${weakTypeOf[A].typeSymbol.asClass} is empty! (whole hierarchy is: ${wholeHierarchy[A]})")

    def isConcrete(symbol: ClassSymbol): Boolean = !(symbol.isAbstractClass || symbol.isTrait)

    case class Member(name: String, term: TermName, tpe: Type)

    object Member {
      def apply(symbol: Symbol) = {
        // Trim because case classes members introduce a trailing space
        val nameStr = symbol.name.decoded.trim
        new Member(nameStr, newTermName(nameStr), symbol.typeSignature)
      }
    }

    /** @return The members of the type `tpe` */
    def listMembers(tpe: Type): List[Member] =
      tpe.typeSymbol.typeSignature.declarations.toList.collect { case x: TermSymbol if x.isVal && x.isCaseAccessor => Member(x) }

    def ops[U <: Adt : c.WeakTypeTag, R[_]](obj: c.Expr[R[U]]) = {
      val anon = newTypeName(c.fresh)
      val wrapper = newTypeName(c.fresh)
      val ctor = newTermName(c.fresh)

      val U = implicitly[c.WeakTypeTag[U]]
      val members = listMembers(U.tpe)

      val objName = U.tpe.typeSymbol.name

      val defGetters = for(member <- members) yield q"def ${member.term}: Rep[${member.tpe}] = adt_select[${U.tpe}, ${member.tpe}]($obj , ${member.name})"

      val paramsCopy = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}] = adt_select[${U.tpe}, ${member.tpe}]($obj , ${member.name})"

      val paramsConstruct = for(member <- members) yield q"${member.term}"

      val defCopy = q"""
        def copy(..${paramsCopy}): Rep[$objName] = $ctor(..${paramsConstruct})
      """

      val variants = U.tpe.baseClasses.drop(1).filter(bc => bc.asClass.toType <:< typeOf[Adt] && bc.asClass.toType != typeOf[Adt])

      // TODO Review this code
      def getFields(params: Seq[Member], root: String, list: List[Tree]): List[Tree] = params match {
        case Nil => 
          if(!variants.isEmpty){
            val variant = root+"$variant"
            q"$variant" :: list
          }else{
            list
          }
        case param +: tail =>
          if (param.tpe <:< typeOf[Adt]) {
            val paramMembers = listMembers(param.tpe)
            val l = getFields(paramMembers, root + param.name + ".", list)
            getFields(tail, root, l)
          } else {
            val name = root + param.name
            getFields(tail, root, q"""$name""" :: list)
          }
      }

      val fieldsObj = getFields(members, "", List())

      val defEqual =
        q"""
          def === (bis: Rep[$objName]): Rep[Boolean] = {
            adt_equal($obj, bis, Seq(..$fieldsObj), Seq(..$fieldsObj))
          }
        """

      val variants2 = wholeHierarchy[U].filter(isConcrete).map(s => s -> newTermName(c.fresh()))

      val paramsFold = for((param, symbol) <- variants2) yield q"val $symbol: (Rep[$param] => Rep[A])"

      val paramsFoldLambda = for((_, symbol) <- variants2) yield q"doLambda($symbol)"

      val paramsFoldName = for(param <- paramsFoldLambda) yield q"$param.asInstanceOf[Rep[$U => A]]"

      val defFold = q"""def fold[A : Manifest](..$paramsFold): Rep[A] = {
            adt_fold($obj, Seq(..$paramsFoldName))
          }
          """

      if (U.tpe.typeSymbol.asClass.isTrait && U.tpe.typeSymbol.asClass.isSealed){
        q"""
        class $anon {
          $defFold
          $defEqual
        }
        class $wrapper extends $anon{}
        new $wrapper
        """
      }else if (U.tpe.typeSymbol.asClass.isCaseClass){
          q"""
          class $anon {
            val $ctor = adt[$objName]
            ..$defGetters
            $defCopy
            $defEqual
          }
          class $wrapper extends $anon{}
          new $wrapper
          """
      }else{
        c.abort(c.enclosingPosition, s"$objName is not a case class or a sealed trait") // TODO support abstract classes as well as traits
      }
    }

    def construct[U <: Adt : c.WeakTypeTag]: c.Tree = {
      val U = weakTypeOf[U]
      val allSubClasses = hierarchy[U]
      if (allSubClasses.size == 1) { // `U` is a leaf in the type hierarchy
        val members = listMembers(U)
        val objName = U.typeSymbol.name
        val paramsDef = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}]"
        val paramsConstruct = for(member <- members) yield q"${member.name} -> ${member.term}"
        val paramsType = for(member <- members) yield tq"Rep[${member.tpe}]"
        val variants = wholeHierarchy[U].filter(isConcrete)
        if (variants.size == 1) {
          q"""
          new ${newTypeName("Function" + paramsType.length)}[..$paramsType, Rep[$objName]] {
            def apply(..$paramsDef) = adt_construct[$objName](..$paramsConstruct)
          }
         """
        } else {
          val variant = variants.indexOf(U.typeSymbol)
          q"""
          new ${newTypeName("Function" + paramsType.length)}[..$paramsType, Rep[$objName]] {
            def apply(..$paramsDef) = adt_construct[$objName](..$paramsConstruct, "$$variant" -> unit($variant))
          }
         """
        }
      } else { // `U` is a sum type
        c.abort(c.enclosingPosition, s"$U is a sum type")
      }
    }
  }
}