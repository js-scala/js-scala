import scala.virtualization.lms.common._

import java.io.PrintWriter

  // object ReifyNewTest {
  //   val o = new { val x = 1; val y = 1 }
  // }
  // println(ReifyNewTest.o.x)
  // type Rep[x] = Exp[x]
  // class ObjDef[T] extends Exp[Row[Rep] with T]
  
  trait JSLiteral extends Base with EmbeddedControls {
    type JSLiteral <: Row[Rep]
    override def __new[T](args: (String, Any)*): T = newJSLiteral(args: _*).asInstanceOf[T]
    def newJSLiteral(args: (String, Any)*): Rep[JSLiteral]
    
    abstract class JSLiteralOps {
      def applyDynamic[T](n: String)(as: AnyRef*): Rep[T]
      def selectDynamic[T](field: String): Rep[T]
    }
    implicit def jsLiteralOps(receiver: Rep[JSLiteral]): JSLiteralOps
  }
  
  trait JSLiteralExp extends JSLiteral with BaseExp {
    trait JSLiteral extends Row[Rep]
    case class JSLiteralExp(args: List[(String, Exp[Any])]) extends Exp[JSLiteral]
    class JSLiteralOpsImpl(val receiver: Exp[JSLiteral]) extends JSLiteralOps {
      def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] =
        sys.error(receiver.toString +
          ".applyDynamic(%1s)(%2s)".format(n, as.mkString("(", ",", ")")))

      def selectDynamic[T](field: String): Rep[T] =
        sys.error(receiver.toString + ".selectDynamic(%1s)".format(field))
    }
    implicit def jsLiteralOps(receiver: Exp[JSLiteral]): JSLiteralOps = new JSLiteralOpsImpl(receiver)
    def newJSLiteral(args: (String, Any)*) : Exp[JSLiteral] = JSLiteralExp(args.toList collect { case (name, arg: Exp[_]) => (name, arg) })
  }
  
//  object Test extends EmbeddedControls {
    
//    case class MyRow(args: List[(String, Any)]) extends Row[Rep] {
      
//    }
    // class ApplyDynamicOps {
    //   def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
    // }      
    // implicit def applyDynamicOps[T <: MyRow](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps    
  
//    override def __new[T](args: (String, Any)*): T = MyRow(args.toList).asInstanceOf[T]
    
//    trait Rep[x]
//    val foo = (new Row[Rep] { val xx = 23; val yy = "y" }) // : Rep[Row[Rep] { val xx: Int; val yy: String }] //.asInstanceOf[Rep[Row[Rep] { val xx: Int; val yy: String }]]
//    val xx = foo.xx 
//  }
  // println(Test.foo)
  
  // object Test  extends EmbeddedControls {
  //   trait Rep[T]
  //   class MyRow extends Row[Rep]
  //   case class MyRow2(args: List[(String, Any)]) extends Row[Rep] {}
  // 
  //   override def __new[T](args: (String, Any)*): T =  MyRow2(args.toList).asInstanceOf[T]
  //   
  //   class ApplyDynamicOps {
  //     def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  //   }
  //   implicit def applyDynamicOps[T <: MyRow](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
  // 
  //   val qual = new MyRow{ val xxx: Rep[Int] = null }
  //   val x: Rep[Int] = qual.xxx // becomes `applyDynamicOps[MyRow{val xxx: Int}](qual).applyDynamic[Int]("xxx")()`
  // }
  // println(Test.qual.xxx)