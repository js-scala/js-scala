package scala.js.macroimpl

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Records {

  def record[U: c.WeakTypeTag](c: Context) =
    c.Expr[Any](new Generator[c.type](c).construct[U])


  def ops[U: c.WeakTypeTag](c: Context)(o: c.Expr[U]) =
    c.Expr[Any](new Generator[c.type](c).ops(o))


  class Generator[C <: Context](val c: C) extends QuasiquoteCompat {
    import c.universe._

    def ops[U: c.WeakTypeTag](obj: c.Expr[U]) = {

      val anon = newTypeName(c.fresh)
      val wrapper = newTypeName(c.fresh)
      val ctor = newTermName(c.fresh)

      val U = implicitly[c.WeakTypeTag[U]]
      val members = U.tpe.members.toList 

      val objName = U.tpe.typeSymbol.name

      // FIXME Is it the right way to retrieve constructor parameters?
      val params = members.filter(_.isPrivate)

      val defGetters = for(param <- params) yield {
        val paramName = param.name
        val paramNameString = param.name.toString
        val paramType = param.typeSignature
        q"def $paramName: Rep[$paramType] = record_select($obj , $paramNameString)"
      }

      val paramsCopy = for(param <- params) yield {
        val paramName = param.name.toTermName
        val paramNameString = param.name.toString
        val paramType = param.typeSignature
        q"val $paramName: Rep[$paramType] = record_select($obj , $paramNameString)"
      }

      val paramsConstruct = for(param <- params) yield {
        val paramName = param.name.toTermName
        q"$paramName"
      }

      val defCopy = q"""
        def copy(..${paramsCopy.reverse}): Rep[$objName] = $ctor(..${paramsConstruct.reverse})
      """

      q"""
      class $anon { 
        val $ctor = record[$objName]
        ..$defGetters
        $defCopy
      }
      class $wrapper extends $anon{}
      new $wrapper
      """
    }

    def construct[U: c.WeakTypeTag] = {

      val U = implicitly[c.WeakTypeTag[U]]
      val members = U.tpe.members.toList
      val objName = U.tpe.typeSymbol.name

      val params = members.filter(_.isPrivate)

      val paramsDef = for(param <- params) yield {
        val paramName = param.name.toTermName
        val paramType = param.typeSignature
        q"val $paramName: Rep[$paramType]"
      }

      val paramsConstruct = for(param <- params) yield {
        val paramName = param.name.toTermName
        val paramNameString = param.name.toString
        q"$paramNameString -> $paramName"
      }

      val paramsName = for(param <- params) yield {
        val paramName = param.name
        q"$paramName"
      }

      val paramsType = for(param <- params) yield {
        val paramType = param.typeSignature
        tq"Rep[$paramType]"
      }

      q"""
        new ${newTypeName("Function"+paramsType.length)} [..${paramsType.reverse}, Rep[$objName]] {
          def apply (..${paramsDef.reverse}) = record_construct[$objName](..${paramsConstruct.reverse})
        }
      """
    }
  }
}