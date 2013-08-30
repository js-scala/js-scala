package scala.js.macroimpl

import scala.language.experimental.macros
import scala.reflect.macros.Context
import sun.management.Flag

object Adts {
  
  trait Adt

  def adt[U: c.WeakTypeTag](c: Context) =
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
    def listMembers(tpe: Type): List[Member] = {
      val fields = tpe.typeSymbol.typeSignature.declarations.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => Member(x) }
      fields
    }

    def getVariant(it: Iterator[Symbol], s: Symbol, i: Int, find: Boolean): (Int, Boolean) = {
          val result = if(!it.hasNext){
              (i, false)

            }else{
              val sc = it.next

              val k = if (s == sc){
                (i, true)

                }else{
                  val nextI: (Int, Boolean) = if (sc.asClass.isTrait){
                      getVariant(sc.asClass.knownDirectSubclasses.toList.init.iterator, s, i, false)

                    }else{
                      (i+1, false)
                    }

                  if(nextI._2){
                    nextI
                  }else{
                    getVariant(it, s, nextI._1, false)
                  }

                }
              k
            }
          result
        }


    def ops[U: c.WeakTypeTag](obj: c.Expr[U]) = {
      val anon = newTypeName(c.fresh)
      val wrapper = newTypeName(c.fresh)
      val ctor = newTermName(c.fresh)

      val U = implicitly[c.WeakTypeTag[U]]
      val members = listMembers(U.tpe)

      val objName = U.tpe.typeSymbol.name

      val defGetters = for(member <- members) yield q"def ${member.term}: Rep[${member.tpe}] = adt_select($obj , ${member.name})"

      val paramsCopy = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}] = adt_select($obj , ${member.name})"

      val paramsConstruct = for(member <- members) yield q"${member.term}"

      val defCopy = q"""
        def copy(..${paramsCopy}): Rep[$objName] = $ctor(..${paramsConstruct})
      """

      val variants = U.tpe.baseClasses.drop(1).filter(bc => bc.asClass.toType <:< typeOf[Adt] && bc.asClass.toType != typeOf[Adt])

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

      val defEqual = q"""
        def === (bis: Rep[$objName]): Rep[Boolean] = {
          adt_equal($obj, bis, Seq(..$fieldsObj), Seq(..$fieldsObj))
        }
      """

        val s = c.weakTypeOf[U].typeSymbol.asClass
        s.typeSignature // SI-7046

        val subClasses = s.knownDirectSubclasses
        
        def getSubClasses (subClasses: List[Symbol], list: List[Symbol]): List[Symbol] = {
            if(subClasses.isEmpty){
              list
            }else{
              val classe = subClasses.head
              if(classe.asClass.isTrait){
                val l = getSubClasses(classe.asClass.knownDirectSubclasses.toList, list)
                getSubClasses(subClasses.tail, l)
              }else{
                getSubClasses(subClasses.tail, classe :: list)
              }
            }
          }

        val allSubClasses = getSubClasses(subClasses.toList, List()).reverse.zipWithIndex.map { case (s, _) => (s, newTermName(c.fresh())) }

        val paramsFold = for((param, symbol) <- allSubClasses) yield q"val $symbol: (Rep[$param] => Rep[A])"

        val paramsFoldLambda = for((param, symbol) <- allSubClasses) yield q"doLambda($symbol)"  //ok (?)  si val = doLambda, erreur de type, à montrer à Olivier

        val paramsFoldName = for(param <- paramsFoldLambda) yield q"$param.asInstanceOf[Rep[${U.tpe} => A]]"

        val defFold = q"""def Fold[A : Manifest](..$paramsFold): Rep[A] = {
            val seqParams = Seq(..$paramsFoldName)
            adt_fold($obj, seqParams)
          }
          """

      if (U.tpe.typeSymbol.asClass.isTrait && U.tpe.typeSymbol.asClass.isSealed){
        q"""
        class $anon extends $U{
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
        c.abort(c.enclosingPosition, s"$objName is not a case class or a sealed trait")
      }
    }

    def construct[U: c.WeakTypeTag] = {

      val U = implicitly[c.WeakTypeTag[U]]
      val members = listMembers(U.tpe)
      val objName = U.tpe.typeSymbol.name

      val paramsDef = for(member <- members) yield q"val ${member.term}: Rep[${member.tpe}]"

      val paramsConstruct = for(member <- members) yield q"${member.name} -> ${member.term}"

      val paramsType = for(member <- members) yield tq"Rep[${member.tpe}]"

        val variants = U.tpe.baseClasses.drop(1).filter(bc => bc.asClass.toType <:< typeOf[Adt] && bc.asClass.toType != typeOf[Adt])

        if(!variants.isEmpty){
          val i = 0
          val it = variants.reverse.iterator

          val variant = getVariant(it, U.tpe.typeSymbol.asClass, 0, false)._1

          q"""
           new ${newTypeName("Function"+paramsType.length)} [..${paramsType}, Rep[$objName]] {
             def apply (..${paramsDef}) = adt_construct[$objName](..${paramsConstruct}, "$$variant" -> unit($variant))
           }
          """

        }else{

          q"""
           new ${newTypeName("Function"+paramsType.length)} [..${paramsType}, Rep[$objName]] {
             def apply (..${paramsDef}) = adt_construct[$objName](..${paramsConstruct})
           }
          """

        }
    }
  }
}