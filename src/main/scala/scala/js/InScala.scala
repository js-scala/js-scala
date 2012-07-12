package scala.js

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait InScala extends Base {
  type Rep[+T] = T

  protected def unit[T:Manifest](x: T) = x
}

trait JSInScala extends JS with NumericOpsInScala with OrderingOpsInScala with EqualInScala with IfThenElseInScala with WhileInScala with BooleanOpsInScala with StringOpsInScala with DynamicInScala with ArraysInScala with JSFunctionsInScala with JSLiteralInScala with JSRegExpsInScala with TupleOpsInScala

trait VariablesInScala extends Variables with ReadVarImplicit with InScala with ImplicitOpsInScala {
  override implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : T = ???
  override def var_new[T:Manifest](init: T)(implicit pos: SourceContext): Var[T] = ???
  override def var_assign[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext): Unit = ???
  override def var_plusequals[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext): Unit = ???
  override def var_minusequals[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext): Unit = ???
  override def var_timesequals[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext): Unit = ???
  override def var_divideequals[T:Manifest](lhs: Var[T], rhs: T)(implicit pos: SourceContext): Unit = ???
}

trait ImplicitOpsInScala extends ImplicitOps with InScala {
  override def implicit_convert[X,Y](x: X)(implicit c: X => Y, mX: Manifest[X], mY: Manifest[Y], pos: SourceContext) : Y = c(x)
}

trait NumericOpsInScala extends NumericOps with VariablesInScala {
  override def numeric_plus[T:Numeric:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext) : T = {
    val t = implicitly[Numeric[T]]
    t.plus(lhs, rhs)
  }
  override def numeric_minus[T:Numeric:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext) : T = {
    val t = implicitly[Numeric[T]]
    t.minus(lhs, rhs)
  }
  override def numeric_times[T:Numeric:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext) : T = {
    val t = implicitly[Numeric[T]]
    t.times(lhs, rhs)
  }
  override def numeric_divide[T:Numeric:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext) : T = {
    val t = implicitly[Numeric[T]]
    (t.toDouble(lhs) / t.toDouble(rhs)).asInstanceOf[T]
  }

}

trait OrderingOpsInScala extends OrderingOps with VariablesInScala {
  override def ordering_lt[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): Boolean = {
    val t = implicitly[Ordering[T]]
    t.lt(lhs, rhs)
  }
  override def ordering_lteq[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): Boolean = {
    val t = implicitly[Ordering[T]]
    t.lteq(lhs, rhs)
  }
  override def ordering_gt[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): Boolean = {
    val t = implicitly[Ordering[T]]
    t.gt(lhs, rhs)
  }
  override def ordering_gteq[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): Boolean = {
    val t = implicitly[Ordering[T]]
    t.gteq(lhs, rhs)
  }
  override def ordering_equiv[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): Boolean = {
    val t = implicitly[Ordering[T]]
    t.equiv(lhs, rhs)
  }
  override def ordering_max[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): T = {
    val t = implicitly[Ordering[T]]
    t.max(lhs, rhs)
  }
  override def ordering_min[T:Ordering:Manifest](lhs: T, rhs: T)(implicit pos: SourceContext): T = {
    val t = implicitly[Ordering[T]]
    t.min(lhs, rhs)
  }
}

trait EqualInScala extends Equal with InScala {
  override def equals[A:Manifest,B:Manifest](a: A, b: B)(implicit pos: SourceContext) : Boolean = a.equals(b)
  override def notequals[A:Manifest,B:Manifest](a: A, b: B)(implicit pos: SourceContext) : Boolean = !a.equals(b)
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
  def string_startswith(s1: String, s2: String) = s1.startsWith(s2)
  def string_todouble(s: String) = s.toDouble
  def string_tofloat(s: String) = s.toFloat
  def string_toint(s: String) = s.toInt

  def array[T:Manifest](xs: T*): Array[T] = Array(xs: _*)
  def array_apply[T:Manifest](a: Array[T], i: Int): T = a(i)
  def array_length[T:Manifest](a: Array[T]): Int = a.length
  def array_update[T:Manifest](a: Array[T], i: Int, x: T): Unit = a(i) = x
  def array_foreach[T:Manifest](a: Array[T], block: T => Unit): Unit = a.foreach(block)
  def array_map[T:Manifest,U:Manifest](a: Array[T], block: T => U): Array[U] = a.map(block)
  def array_flatMap[T:Manifest,U:Manifest](a: Array[T], block: T => Array[U]): Array[U] = a.flatMap(block andThen (x => x : scala.collection.GenTraversableOnce[U]))
  def array_filter[T:Manifest](a: Array[T], block: T => Boolean): Array[T] = a.filter(block)

  def string_regexp(r: String) = r.r
}

trait IfThenElseInScala extends IfThenElse with InScala {
  override def __ifThenElse[T:Manifest](cond: Boolean, thenp: => T, elsep: => T)(implicit pos: SourceContext): T =
    OriginalOps.ifThenElse[T](cond, thenp, elsep)
}

trait WhileInScala extends While with InScala {
  override def __whileDo(cond: => Boolean, body: => Unit)(implicit pos: SourceContext) = OriginalOps.whileDo(cond, body)
}

trait BooleanOpsInScala extends BooleanOps with InScala {
  override def boolean_negate(lhs: Boolean)(implicit pos: SourceContext) : Boolean = OriginalOps.boolean_negate(lhs)
  override def boolean_and(lhs: Boolean, rhs: Boolean)(implicit pos: SourceContext) : Boolean = OriginalOps.boolean_and(lhs, rhs)
  override def boolean_or(lhs: Boolean, rhs: Boolean)(implicit pos: SourceContext) : Boolean = OriginalOps.boolean_or(lhs, rhs)
}

trait StringOpsInScala extends StringOps with InScala {
  override def string_plus(s: Any, o: Any)(implicit pos: SourceContext): String = OriginalOps.string_plus(s, o)
  override def string_trim(s: String)(implicit pos: SourceContext) : String = OriginalOps.string_trim(s)
  override def string_split(s: String, separators: String)(implicit pos: SourceContext) : Array[String] = OriginalOps.string_split(s, separators)
  override def string_valueof(a: Any)(implicit pos: SourceContext) = OriginalOps.string_valueof(a)
  override def string_startswith(s1: String, s2: String)(implicit pos: SourceContext): Boolean = OriginalOps.string_startswith(s1, s2)
  override def string_todouble(s: String)(implicit pos: SourceContext): Double = OriginalOps.string_todouble(s)
  override def string_tofloat(s: String)(implicit pos: SourceContext): Float = OriginalOps.string_tofloat(s)
  override def string_toint(s: String)(implicit pos: SourceContext): Int = OriginalOps.string_toint(s)
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
  override implicit def doLambda[A:Manifest,B:Manifest](fun: A => B)(implicit pos: SourceContext): A => B = fun
  override def doApply[A:Manifest,B:Manifest](fun: A => B, arg: A)(implicit pos: SourceContext): B = fun(arg)
}

trait JSLiteralInScala extends JSLiteral with InScala {
  def newJSLiteral(args: (String, JSLiteral => (t forSome{type t}))*): JSLiteral = ???
  implicit def jsLiteralOps(receiver: JSLiteral): JSLiteralOps = ???
}

trait JSRegExpsInScala extends JSRegExps with InScala {
  import scala.util.matching.Regex

  override def string_regexp(r: String): Regex =
    OriginalOps.string_regexp(r)
  override def regexp_test(re: Regex, str: String): Boolean =
    re.findFirstIn(str) != None
  override def string_search(str: String, re: Regex): Int =
    re.findFirstMatchIn(str).map(_.start).getOrElse(-1)
}

trait TupleOpsInScala extends TupleOps with InScala {
  override implicit def make_tuple2[A:Manifest,B:Manifest](t: (A, B))(implicit pos: SourceContext) : (A,B) = t
  override implicit def make_tuple3[A:Manifest,B:Manifest,C:Manifest](t: (A, B, C))(implicit pos: SourceContext) : (A,B,C) = t
  override implicit def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](t: (A, B, C, D))(implicit pos: SourceContext) : (A,B,C,D) = t
  override implicit def make_tuple5[A:Manifest,B:Manifest,C:Manifest,D:Manifest,E:Manifest](t: (A, B, C, D, E))(implicit pos: SourceContext) : (A,B,C,D,E) = t

  override def tuple2_get1[A:Manifest](t: (A,_))(implicit pos: SourceContext) : A = t._1
  override def tuple2_get2[B:Manifest](t: (_,B))(implicit pos: SourceContext) : B = t._2

  override def tuple3_get1[A:Manifest](t: (A,_,_))(implicit pos: SourceContext) : A = t._1
  override def tuple3_get2[B:Manifest](t: (_,B,_))(implicit pos: SourceContext) : B = t._2
  override def tuple3_get3[C:Manifest](t: (_,_,C))(implicit pos: SourceContext) : C = t._3

  override def tuple4_get1[A:Manifest](t: (A,_,_,_))(implicit pos: SourceContext) : A = t._1
  override def tuple4_get2[B:Manifest](t: (_,B,_,_))(implicit pos: SourceContext) : B = t._2
  override def tuple4_get3[C:Manifest](t: (_,_,C,_))(implicit pos: SourceContext) : C = t._3
  override def tuple4_get4[D:Manifest](t: (_,_,_,D))(implicit pos: SourceContext) : D = t._4

  override def tuple5_get1[A:Manifest](t: (A,_,_,_,_))(implicit pos: SourceContext) : A = t._1
  override def tuple5_get2[B:Manifest](t: (_,B,_,_,_))(implicit pos: SourceContext) : B = t._2
  override def tuple5_get3[C:Manifest](t: (_,_,C,_,_))(implicit pos: SourceContext) : C = t._3
  override def tuple5_get4[D:Manifest](t: (_,_,_,D,_))(implicit pos: SourceContext) : D = t._4
  override def tuple5_get5[E:Manifest](t: (_,_,_,_,E))(implicit pos: SourceContext) : E = t._5
}

trait JSProxyInScala extends JSProxyBase with InScala {
  def repProxy[T<:AnyRef](x: Rep[T])(implicit m: Manifest[T]): T = x
}

trait JSTraitsInScala extends JSTraits with JSProxyInScala {
  def create[T<:AnyRef:Manifest](): T =
    throw new RuntimeException("don't know how to create " + implicitly[Manifest[T]].erasure.getName)
  def register[T<:AnyRef:Manifest](outer: AnyRef): Factory[T] =
    new Factory[T] {
      override def apply() = create[T]()
    }
}

trait DomsInScala extends Doms with JSProxyInScala {
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
        resetStroke()
	graphics.translate(0, 30)
	draw()
      }
    }
    frame.setTitle("Canvas as Java's Graphics2D")
    frame.setBackground(Color.white)
    frame.setForeground(Color.white)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(800, 300)
    frame.setVisible(true)
  }

  val strokes : Stack[Float] = new Stack()
  private var strokeWidth = 0.0f
  private def resetStroke() {
    strokeWidth = 1.0f
    updateStroke()
  }
  private def adjustStroke(sx: Double, sy: Double) {
    strokeWidth = strokeWidth / Math.min(sx, sy).toFloat
    updateStroke()
  }
  private def updateStroke() {
    graphics.setStroke(new BasicStroke(strokeWidth))
  }
  private def restoreStroke() {
    strokeWidth = strokes.pop()
  }
  private def saveStroke() {
    strokes.push(strokeWidth)
  }

  class ElementInScala extends Element {
    def getElementById(id: String) : ElementInScala = new CanvasInScala
  }
  class CanvasInScala extends ElementInScala with Canvas {
    def getContext(context: String) : ContextInScala = new ContextInScala
  }
  class ContextInScala extends Context {
    def save(): Unit = {
      transforms.push(graphics.getTransform)
      saveStroke()
    }
    def restore(): Unit = {
      graphics.setTransform(transforms.pop())
      restoreStroke()
    }
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
    def scale(sx: Double, sy: Double): Unit = {
      graphics.scale(sx, sy)
      adjustStroke(sx, sy)
    }
    def rotate(theta: Double): Unit =
      graphics.rotate(theta)
    def translate(tx: Int, ty: Int): Unit =
      graphics.translate(tx, ty)
    def closePath(): Unit = {}
    def stroke(): Unit = {}
  }
  val document = new ElementInScala
}
