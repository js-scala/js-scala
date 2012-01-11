package scala.js

import scala.virtualization.lms.common._

import javassist._
import javassist.expr._
import scala.collection.JavaConversions._

import java.io.PrintWriter

trait JSClasses extends JSClassProxyBase with Functions {
  trait ClassFactory[+T] {
    def apply(args: Rep[Any]*): Rep[T]
  }
  def registerClass[T<:AnyRef:Manifest](outer: AnyRef): ClassFactory[T]
  abstract override def doLambda[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B]): Rep[A => B] =
    bindThis(super.doLambda(fun))
  def bindThis[A:Manifest,B:Manifest](fun: Rep[A => B]): Rep[A => B]
}

trait JSClassesExp extends JSClasses with JSClassProxyExp {
  trait Constructor[+T]

  case class MethodTemplate(name: String, params: List[Sym[Any]], body: Exp[Any])
  case class ParentTemplate(constructor: Exp[Any], instance: Exp[Any])

  case class This[T:Manifest]() extends Exp[T]

  case class ClassTemplate[T:Manifest](parent: Option[ParentTemplate], methods: List[MethodTemplate]) extends Def[Constructor[T]]
  case class New[T:Manifest](constructor: Exp[Constructor[T]], args: List[Rep[Any]]) extends Def[T]

  case class BindThis[A:Manifest,B:Manifest](fun: Exp[A => B]) extends Def[A => B]
  override def bindThis[A:Manifest,B:Manifest](fun: Rep[A => B]): Rep[A => B] =
    reflectEffect(BindThis(fun))

  override def registerClass[T<:AnyRef:Manifest](outer: AnyRef) = {
    val constructor = registerInternal[T](outer)
    new ClassFactory[T] {
      override def apply(args: Rep[Any]*) = create[T](constructor, args.toList)
    }
  }

  private def create[T<:AnyRef:Manifest](constructor: Exp[Constructor[T]], args: List[Rep[Any]]): Exp[T] =
    reflectEffect(New(constructor, args))

  private var registered: Map[String, Exp[Constructor[Any]]] = Map()
  private def registerInternal[T<:AnyRef:Manifest](outer: AnyRef) : Exp[Constructor[T]] = {
    val m = implicitly[Manifest[T]]
    val clazz = m.erasure
    val key = clazz.getName

    registered.get(key) match {
       case Some(constructor) => return constructor.asInstanceOf[Exp[Constructor[T]]]
       case None => ()
    }

    val parentConstructor =
      if (clazz.getSuperclass.getName == "java.lang.Object") None
      else Some(registerInternal[AnyRef](outer)(Manifest.classType(clazz.getSuperclass)))
    val parent = parentConstructor.map(c => ParentTemplate(c, create[AnyRef](c, List[Rep[Any]]())))

    def rename(className: String) = className + "$bis"
    val initMethodName = "$init$"

    val bisKey = rename(key)
    val cp = ClassPool.getDefault
    cp.insertClassPath(new ClassClassPath(clazz))
    val cc = cp.get(key)

    def isFieldAccess(method: String) = {
      try {
        cc.getField(method)
        true
      } catch {
        case e: NotFoundException => false
      }
    }
    def doReifyMethod(method: String) =
      !method.contains("$") && !isFieldAccess(method)
    def doReifyField(field: String) =
      field != "$outer"

    var bisClazz = null: Class[_]
    try {
      bisClazz = Class.forName(bisKey)
    } catch {
      case e: ClassNotFoundException => ()
    }

    if (bisClazz == null) {
      val deps = cc.getRefClasses.map(_.asInstanceOf[String]).filter(_.startsWith(key + "$"))
      cc.setName(bisKey)

      val superMethod = CtNewMethod.make(
        "protected Object " + superMethodName + "(String name, Object[] args) { return null; }",
        cc)
      cc.addMethod(superMethod)

      for (method <- cc.getDeclaredMethods)
        if (Modifier.isPrivate(method.getModifiers) || Modifier.isPackage(method.getModifiers)) {
          method.setModifiers(Modifier.setProtected(method.getModifiers))
        }

      for (field <- cc.getDeclaredFields;
           if doReifyField(field.getName)) {
        try {
          cc.getDeclaredMethod(field.getName)
        } catch {
          case e: NotFoundException =>
            cc.addMethod(CtNewMethod.getter(
              field.getName, field))
        }
        try {
          cc.getDeclaredMethod(
            updateMethodFromField(field.getName),
            Array(field.getType))
        } catch {
          case e: NotFoundException =>
            cc.addMethod(CtNewMethod.setter(
              updateMethodFromField(field.getName), field))
        }
      }
      def fieldAccess(field: String) =
        "$_ = $0." + field + "();"
      def fieldUpdate(field: String) =
        "$0." + updateMethodFromField(field) + "($1);"
      def editField(f: expr.FieldAccess) = {
        if (f.getClassName == bisKey && doReifyField(f.getFieldName)) {
          f.replace((if (f.isReader) fieldAccess _ else fieldUpdate _) (f.getFieldName))
        }
      }
      val exprEditor = new ExprEditor() {
        override def edit(f: expr.FieldAccess) = editField(f)
        override def edit(m: expr.MethodCall) {
          if (m.isSuper) {
            val name = m.getMethodName
            val n = m.getMethod.getParameterTypes.length
            var src = "Object[] args = new Object[" + n + "];"
            for (i <- 1 to n)
              src += "args[" + (i-1) + "] = $" + i + ";"
            src += "$_ = " + superMethodName + "(\"" + name + "\", args);"
            if (m.getMethod.getReturnType == CtClass.voidType)
              src = src.replace("$_ =", "")
            m.replace(src)
          } else if (m.getClassName == bisKey) {
            // this ensures that calls to formerly private methods get
            // re-written to use invokevirtual instead of invokespecial
            val n = m.getMethod.getParameterTypes.length
            val args = (1 to n).map("$" + _).mkString(", ")
            var src = "$0." + m.getMethodName + "(" + args + ");"
            if (m.getMethod.getReturnType != CtClass.voidType)
              src = "$_ = " + src
            m.replace(src)
          }
        }
        override def edit(c: expr.ConstructorCall) {
          if (c.isSuper && c.getConstructor.getName != "Object") {
            val n = c.getConstructor.getParameterTypes.length-1
            var src = "Object[] args = new Object[" + n + "];"
            for (i <- 1 to n)
              src += "args[" + (i-1) + "] = $" + (i+1) + ";"
            src += superMethodName + "(\"$init$\", args);"
            c.replace(src)
          }
        }
      }

      val ctConstructor = new CtConstructor((cc.getDeclaredConstructors())(0), cc, null)
      ctConstructor.instrument(exprEditor)
      val ctConstructorMethod = ctConstructor.toMethod(initMethodName, cc)
      cc.addMethod(ctConstructorMethod)

      for (method <- cc.getDeclaredMethods)
        if (doReifyMethod(method.getName))
          method.instrument(exprEditor)

      if (!deps.isEmpty) {
        val ccSerializable = cp.get("java.io.Serializable")
        cc.addInterface(ccSerializable)

        val map = new ClassMap()
        map.put(key, bisKey)

        val depExprEditor = new ExprEditor() {
          override def edit(f: expr.FieldAccess) = editField(f)
        }

        val ccDeps = deps.map(depKey => {
          val depcc = cp.get(depKey)
          val depBisKey = rename(depKey)
          depcc.setName(depBisKey)
          map.put(depKey, depBisKey)
          depcc
        })

        cc.replaceClassName(map)
        ccDeps.foreach(depcc => {
          depcc.replaceClassName(map)
          depcc.instrument(depExprEditor)
          depcc.addInterface(ccSerializable)
          depcc.toClass()
        })
      }

      bisClazz = cc.toClass()
    }

    val jConstructor = (bisClazz.getDeclaredConstructors())(0)
    val jConstructorMethod = bisClazz.getDeclaredMethod(initMethodName, jConstructor.getParameterTypes: _*)
    val constructorTemplate = {
      val paramTypes = jConstructorMethod.getParameterTypes
      val n = paramTypes.length
      val args = (1 to (n-1)).toList.map(i => paramTypes(i).getName match {
        case "scala.reflect.Manifest" => manifest[Any]
        case _ => fresh[Any]
      })
      val allArgs = (outer::args).toArray
      val params = args.filter(_.isInstanceOf[Sym[_]]).map(_.asInstanceOf[Sym[Any]])
      val self = repMasqueradeProxy(bisClazz, This[T](), parentConstructor, outer, Set(initMethodName))
      MethodTemplate(initMethodName, params, reifyEffects(jConstructorMethod.invoke(self, allArgs: _*).asInstanceOf[Exp[Any]]))
    }

    val methods = 
      for (method <- bisClazz.getDeclaredMethods.toList;
           if (doReifyMethod(method.getName)))
      yield {
        val n = method.getParameterTypes.length
        val params = (1 to n).toList.map(_ => fresh[Any])
        val args = params.toArray
        val self = repMasqueradeProxy(bisClazz, This[T](), parentConstructor, outer, Set(method.getName))
        MethodTemplate(method.getName, params, reifyEffects(method.invoke(self, args: _*).asInstanceOf[Exp[Any]]))
      }
    
    val constructor = ClassTemplate[T](parent, constructorTemplate::methods) : Exp[Constructor[T]]
    registered = registered.updated(key, constructor)
    constructor
  }

  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case MethodTemplate(_, params, body) => syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MethodTemplate(_, params, body) => params.flatMap(syms) ::: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MethodTemplate(_, params, body) => freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait JSGenClasses extends JSGenBase with JSGenClassProxy {
  val IR: JSClassesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ClassTemplate(parentTemplate, methodTemplates @ MethodTemplate(_, params, _)::_) =>
      stream.println("var " + quote(sym) + " = function" + params.map(quote).mkString("(", ",", ")") + "{")
      var initCall = "this.$init$" + params.map(quote).mkString("(", ",", ")")
      if (!params.isEmpty) {
        initCall = "if (arguments.length != 0) {\n" + initCall + "\n}"
      }
      stream.println(initCall)
      stream.println("}")
      parentTemplate.foreach(pt => stream.println(quote(sym) + ".prototype = " + quote(pt.instance)))
      for (MethodTemplate(name, params, body) <- methodTemplates) {
	stream.println(quote(sym) + ".prototype." + name + " = function" + params.map(quote).mkString("(", ",", ")") + " {")
	emitBlock(body)
	stream.println("return " + quote(getBlockResult(body)))
	stream.println("}")
      }
    case New(constructor, args) =>
      emitValDef(sym, "new " + quote(constructor) + args.map(quote).mkString("(", ", ", ")"))
    case BindThis(fun) =>
      emitValDef(sym, quote(fun) + ".bind(this)")
    case _ => super.emitNode(sym, rhs)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case This() => "this"
    case _ => super.quote(x)
  }
}
