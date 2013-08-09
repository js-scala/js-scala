package scala.js.macroimpl

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Records {
  
  trait Record

  def record[U: c.WeakTypeTag](c: Context) =
    c.Expr[Any](new Generator[c.type](c).construct[U])


  def ops[U: c.WeakTypeTag](c: Context)(o: c.Expr[U]) =
    c.Expr[Any](new Generator[c.type](c).ops(o))


  class Generator[C <: Context](val c: C) extends QuasiquoteCompat {
    import c.universe._

    case class Member(name: String, term: TermName, tpe: Type)

    object Member {
      def apply(symbol: Symbol) = {
        // Trim because case classes members introduce a trailing space
        val nameStr = symbol.name.decoded.trim
        new Member(nameStr, newTermName(nameStr), symbol.typeSignature)
      }
    }

    /** @return The members of the type `tpe` */
    def listMembers(tpe: Type): Seq[Member] =
      (for (member <- tpe.members if member.isPrivate) yield Member(member)).to[Seq]

    def ops[U: c.WeakTypeTag](obj: c.Expr[U]) = {
      val anon = newTypeName(c.fresh)
      val wrapper = newTypeName(c.fresh)
      val ctor = newTermName(c.fresh)

      val U = implicitly[c.WeakTypeTag[U]]
      val members = listMembers(U.tpe)

      val objName = U.tpe.typeSymbol.name

      val defGetters = for(member <- members) yield q"def ${member.term}: Rep[${member.tpe}] = record_select($obj , ${member.name})"

      val paramsCopy = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}] = record_select($obj , ${member.name})"

      val paramsConstruct = for(member <- members) yield q"${member.term}"

      val defCopy = q"""
        def copy(..${paramsCopy.reverse}): Rep[$objName] = $ctor(..${paramsConstruct.reverse})
      """

      def getFields(params: Seq[Member], root: String, list: List[Tree]): List[Tree] = params match {
        case Nil => list
        case param +: tail =>
          if (param.tpe <:< typeOf[Record]) {
            val paramMembers = listMembers(param.tpe)
            val l = getFields(paramMembers, root + param.name + ".", list)
            getFields(tail, root, l)
          } else {
            val name = root + param.name
            getFields(tail, root, q"""$name""" :: list)
          }
      }

      val fieldsObj = getFields(members, "", List())

      val defEqual = q"""
        def === (bis: Rep[$objName]): Rep[Boolean] = {
          record_equal($obj, bis, Seq(..$fieldsObj), Seq(..$fieldsObj))
        }
      """

      q"""
      class $anon { 
        val $ctor = record[$objName]
        ..$defGetters
        $defCopy
        $defEqual
      }
      class $wrapper extends $anon{}
      new $wrapper
      """
    }

    def construct[U: c.WeakTypeTag] = {

      val U = implicitly[c.WeakTypeTag[U]]
      val members = listMembers(U.tpe)
      val objName = U.tpe.typeSymbol.name

      val paramsDef = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}]"

      val paramsConstruct = for(member <- members) yield q"${member.name} -> ${member.term}"

      val paramsType = for(member <- members) yield tq"Rep[${member.tpe}]"

      q"""
        new ${newTypeName("Function"+paramsType.length)} [..${paramsType.reverse}, Rep[$objName]] {
          def apply (..${paramsDef.reverse}) = record_construct[$objName](..${paramsConstruct.reverse})
        }
      """
    }
  }
}