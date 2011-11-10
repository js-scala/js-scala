package scala.js

import scala.virtualization.lms.common._

trait InScala extends Base {
  type Rep[+T] = T

  protected def unit[T:Manifest](x: T) = x
}

trait JSInScala extends JS with NumericOpsInScala with OrderingOpsInScala with EqualInScala with IfThenElseInScala with WhileInScala with BooleanOpsInScala with StringOpsInScala with DynamicInScala with ArraysInScala with JSFunctionsInScala with JSLiteralInScala with TupleOpsInScala

trait VariablesInScala extends Variables with ReadVarImplicit with InScala with ImplicitOpsInScala {
  implicit def readVar[T:Manifest](v: Var[T]) : T = ???
  def var_new[T:Manifest](init: T): Var[T] = ???
  def var_assign[T:Manifest](lhs: Var[T], rhs: T): Unit = ???
  def var_plusequals[T:Manifest](lhs: Var[T], rhs: T): Unit = ???
  def var_minusequals[T:Manifest](lhs: Var[T], rhs: T): Unit = ???
}

trait ImplicitOpsInScala extends ImplicitOps with InScala {
  def implicit_convert[X,Y](x: X)(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y]) : Y = c(x)
}

trait NumericOpsInScala extends NumericOps with VariablesInScala {
  def numeric_plus[T:Numeric:Manifest](lhs: T, rhs: T) : T = {
    val t = implicitly[Numeric[T]]
    t.plus(lhs, rhs)
  }
  def numeric_minus[T:Numeric:Manifest](lhs: T, rhs: T) : T = {
    val t = implicitly[Numeric[T]]
    t.minus(lhs, rhs)
  }
  def numeric_times[T:Numeric:Manifest](lhs: T, rhs: T) : T = {
    val t = implicitly[Numeric[T]]
    t.times(lhs, rhs)
  }
  def numeric_divide[T:Numeric:Manifest](lhs: T, rhs: T) : T = {
    val t = implicitly[Numeric[T]]
    (t.toDouble(lhs) / t.toDouble(rhs)).asInstanceOf[T]
  }

}

trait OrderingOpsInScala extends OrderingOps with VariablesInScala {
  def ordering_lt[T:Ordering:Manifest](lhs: T, rhs: T): Boolean = {
    val t = implicitly[Ordering[T]]
    t.lt(lhs, rhs)
  }
  def ordering_lteq[T:Ordering:Manifest](lhs: T, rhs: T): Boolean = {
    val t = implicitly[Ordering[T]]
    t.lteq(lhs, rhs)
  }
  def ordering_gt[T:Ordering:Manifest](lhs: T, rhs: T): Boolean = {
    val t = implicitly[Ordering[T]]
    t.gt(lhs, rhs)
  }
  def ordering_gteq[T:Ordering:Manifest](lhs: T, rhs: T): Boolean = {
    val t = implicitly[Ordering[T]]
    t.gteq(lhs, rhs)
  }
  def ordering_equiv[T:Ordering:Manifest](lhs: T, rhs: T): Boolean = {
    val t = implicitly[Ordering[T]]
    t.equiv(lhs, rhs)
  }
  def ordering_max[T:Ordering:Manifest](lhs: T, rhs: T): T = {
    val t = implicitly[Ordering[T]]
    t.max(lhs, rhs)
  }
  def ordering_min[T:Ordering:Manifest](lhs: T, rhs: T): T = {
    val t = implicitly[Ordering[T]]
    t.min(lhs, rhs)
  }
}

trait EqualInScala extends Equal with InScala {
  def equals[A:Manifest,B:Manifest](a: A, b: B) : Boolean = a.equals(b)
  def notequals[A:Manifest,B:Manifest](a: A, b: B) : Boolean = !a.equals(b)
}

object OriginalOps {
  def ifThenElse[T](cond: Boolean, thenp: => T, elsep: => T): T =
    if (cond) thenp else elsep
  def whileDo(cond: => Boolean, body: => Unit) =
    while (cond) body

  def boolean_negate(lhs: Boolean) : Boolean = !lhs
  def boolean_and(lhs: Boolean, rhs: Boolean) : Boolean = lhs && rhs
  def boolean_or(lhs: Boolean, rhs: Boolean) : Boolean = lhs || rhs

  def string_plus(s: Any, o: Any): String = string_valueof(s) + string_valueof(o)
  def string_trim(s: String) : String = s.trim()
  def string_split(s: String, separators: String) : Array[String] = s.split(separators)
  def string_valueof(a: Any) = String.valueOf(a)

  def array[T:Manifest](xs: T*): Array[T] = Array(xs: _*)
  def array_apply[T:Manifest](a: Array[T], i: Int): T = a(i)
  def array_length[T:Manifest](a: Array[T]): Int = a.length
  def array_update[T:Manifest](a: Array[T], i: Int, x: T): Unit = a(i) = x
  def array_foreach[T:Manifest](a: Array[T], block: T => Unit): Unit = a.foreach(block)
  def array_map[T:Manifest,U:Manifest](a: Array[T], block: T => U): Array[U] = a.map(block)
  def array_flatMap[T:Manifest,U:Manifest](a: Array[T], block: T => Array[U]): Array[U] = a.flatMap(block andThen (x => x : scala.collection.GenTraversableOnce[U]))
  def array_filter[T:Manifest](a: Array[T], block: T => Boolean): Array[T] = a.filter(block)
}

trait IfThenElseInScala extends IfThenElse with InScala {
  def __ifThenElse[T:Manifest](cond: Boolean, thenp: => T, elsep: => T): T =
    OriginalOps.ifThenElse[T](cond, thenp, elsep)
}

trait WhileInScala extends While with InScala {
  def __whileDo(cond: => Boolean, body: => Unit) = OriginalOps.whileDo(cond, body)
}

trait BooleanOpsInScala extends BooleanOps with InScala {
  def boolean_negate(lhs: Boolean) : Boolean = OriginalOps.boolean_negate(lhs)
  def boolean_and(lhs: Boolean, rhs: Boolean) : Boolean = OriginalOps.boolean_and(lhs, rhs)
  def boolean_or(lhs: Boolean, rhs: Boolean) : Boolean = OriginalOps.boolean_or(lhs, rhs)
}

trait StringOpsInScala extends StringOps with InScala {
  def string_plus(s: Any, o: Any): String = OriginalOps.string_plus(s, o)
  def string_trim(s: String) : String = OriginalOps.string_trim(s)
  def string_split(s: String, separators: String) : Array[String] = OriginalOps.string_split(s, separators)
  def string_valueof(a: Any) = OriginalOps.string_valueof(a)
}

trait DynamicInScala extends DynamicBase with InScala {
  def dynamic(x: Any) = ???
  def newDynamic(constructor: String)(args: Any*) = ???
  def inlineDynamic(code: String) = ???
}

trait ArraysInScala extends Arrays with InScala {
  def array[T:Manifest](xs: T*): Array[T] = OriginalOps.array(xs: _*)
  def array_apply[T:Manifest](a: Array[T], i: Int): T = OriginalOps.array_apply(a, i)
  def array_length[T:Manifest](a: Array[T]): Int = OriginalOps.array_length(a)
  def array_update[T:Manifest](a: Array[T], i: Int, x: T): Unit = OriginalOps.array_update(a, i, x)
  def array_foreach[T:Manifest](a: Array[T], block: T => Unit): Unit = OriginalOps.array_foreach(a, block)
  def array_map[T:Manifest,U:Manifest](a: Array[T], block: T => U): Array[U] = OriginalOps.array_map(a, block)
  def array_flatMap[T:Manifest,U:Manifest](a: Array[T], block: T => Array[U]): Array[U] = OriginalOps.array_flatMap(a, block)
  def array_filter[T:Manifest](a: Array[T], block: T => Boolean): Array[T] = OriginalOps.array_filter(a, block)
  def array_join[T:Manifest](a: Array[T], s: String): String = a.mkString(s)
  def range_foreach(r: Range, block: Int => Unit): Unit = (r.a to r.b).foreach(block)
  def range_map[U:Manifest](r: Range, block: Int => U): Array[U] = (r.a to r.b).map(block).toArray
  def range_flatMap[U:Manifest](r: Range, block: Int => Array[U]): Array[U] = (r.a to r.b).flatMap(block andThen (x => x : scala.collection.GenTraversableOnce[U])).toArray
  def range_filter(r: Range, block: Int => Boolean): Array[Int] = (r.a to r.b).filter(block).toArray
}

trait JSFunctionsInScala extends JSFunctions with InScala {
  implicit def doLambda[A:Manifest,B:Manifest](fun: A => B): A => B = fun
  def doApply[A:Manifest,B:Manifest](fun: A => B, arg: A): B = fun(arg)
}

trait JSLiteralInScala extends JSLiteral with InScala {
  def newJSLiteral(args: (String, Rep[JSLiteral] => (Rep[t] forSome{type t}))*): JSLiteral = ???
  implicit def jsLiteralOps(receiver: JSLiteral): JSLiteralOps = ???
}

trait TupleOpsInScala extends TupleOps with InScala {
  implicit def make_tuple2[A:Manifest,B:Manifest](t: (A, B)) : (A,B) = t
  implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (A, B, C)) : (A,B,C) = t
  implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (A, B, C, D)) : (A,B,C,D) = t
  implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (A, B, C, D, E)) : (A,B,C,D,E) = t

  def tuple2_get1[A:Manifest](t: (A,_)) : A = t._1
  def tuple2_get2[B:Manifest](t: (_,B)) : B = t._2

  def tuple3_get1[A:Manifest](t: (A,_,_)) : A = t._1
  def tuple3_get2[B:Manifest](t: (_,B,_)) : B = t._2
  def tuple3_get3[C:Manifest](t: (_,_,C)) : C = t._3

  def tuple4_get1[A:Manifest](t: (A,_,_,_)) : A = t._1
  def tuple4_get2[B:Manifest](t: (_,B,_,_)) : B = t._2
  def tuple4_get3[C:Manifest](t: (_,_,C,_)) : C = t._3
  def tuple4_get4[D:Manifest](t: (_,_,_,D)) : D = t._4

  def tuple5_get1[A:Manifest](t: (A,_,_,_,_)) : A = t._1
  def tuple5_get2[B:Manifest](t: (_,B,_,_,_)) : B = t._2
  def tuple5_get3[C:Manifest](t: (_,_,C,_,_)) : C = t._3
  def tuple5_get4[D:Manifest](t: (_,_,_,D,_)) : D = t._4
  def tuple5_get5[E:Manifest](t: (_,_,_,_,E)) : E = t._5
}

trait DomsInScala extends Doms with InScala {
  import java.awt._
  import java.awt.geom._
  import javax.swing.JFrame
  import scala.collection.mutable.Stack

  val transforms : Stack[AffineTransform] = new Stack()
  var point : Point2D = new Point2D.Double(0, 0)
  var pointTransform = new AffineTransform()

  var graphics : Graphics2D = null
  def init(draw: () => Unit) {
    val frame = new JFrame() {
      override def paint(g : Graphics) {
	graphics = g.asInstanceOf[Graphics2D]
	graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
	graphics.setPaint(Color.black)
	graphics.translate(0, 30)
	draw()
      }
    }
    frame.setBackground(Color.white)
    frame.setForeground(Color.white)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(800, 300)
    frame.setVisible(true)
  }

  class ElementInScala extends Element with ElementOps {
    def getElementById(id: String) : ElementInScala = new CanvasInScala
  }
  class CanvasInScala extends ElementInScala with Canvas with CanvasOps {
    def getContext(context: String) : ContextInScala = new ContextInScala
  }
  class ContextInScala extends ElementInScala with Context with ContextOps {
    def save(): Unit =
      transforms.push(graphics.getTransform)
    def restore(): Unit =
      graphics.setTransform(transforms.pop())
    def moveTo(x: Int, y: Int) {
      point = new Point2D.Double(x, y)
      pointTransform = graphics.getTransform
    }
    def lineTo(x: Int, y: Int): Unit = {
      val start = graphics.getTransform.inverseTransform(pointTransform.transform(point, null), null)
      val end = new Point2D.Double(x, y)
      graphics.draw(new Line2D.Double(start, end))

      point = end
      pointTransform = graphics.getTransform
    }
    def scale(sx: Double, sy: Double): Unit =
      graphics.scale(sx, sy)
    def rotate(theta: Double): Unit =
      graphics.rotate(theta)
    def translate(tx: Int, ty: Int): Unit =
      graphics.translate(tx, ty)
    def closePath(): Unit = {}
    def stroke(): Unit = {}
  }
  val document = new ElementInScala
  implicit def elementOps(x: Element): ElementOps = new ElementInScala
  implicit def canvasOps(x: Canvas): CanvasOps = new CanvasInScala
  implicit def contextOps(x: Context): ContextOps = new ContextInScala
  implicit def asOps(x: X forSome {type X}): AsOps = new AsOps {
    def as[T]: T = x.asInstanceOf[T]
  }
}
