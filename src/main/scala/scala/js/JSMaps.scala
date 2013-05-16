package scala.js

import scala.js._
import scala.virtualization.lms.common._

/** Support for scala map operators in javascript.
 *
 *  @author Lucas Satabin
 */
trait JSMaps extends Base {

  object JSMap {
    def apply[K:JSKey,V:Manifest]() = map_new[K,V]()
  }

  /* only some types are authorized as keys to avoid problems in javascript
   * authorized keys are of type:
   *  - String
   *  - Char
   *  - numeric types (Int, Long, Float, Double, Byte, ...)
   *  - Boolean
   */
  abstract class JSKey[A:Manifest] extends Manifest[A] {
    def runtimeClass = implicitly[Manifest[A]].runtimeClass
  }
  object JSKey {
    implicit val stringKey = new JSKey[String] {}
    implicit val charKey = new JSKey[Char] {}
    implicit val intKey = new JSKey[Int] {}
    implicit val longKey = new JSKey[Long] {}
    implicit val floatKey = new JSKey[Float] {}
    implicit val doubleKey = new JSKey[Double] {}
    implicit val boolKey = new JSKey[Boolean] {}
  }

  implicit def repMapToMapOps[K:JSKey,V:Manifest](m: Rep[Map[K,V]]) = new mapOpsCls(m)

  class mapOpsCls[K:JSKey,V:Manifest](m: Rep[Map[K,V]]) {
    val keyType = implicitly[Manifest[K]]
    val valueType = implicitly[Manifest[V]]
    def apply(k: Rep[K]) = map_apply(m, k)
    def get(k: Rep[K]) = map_get(m, k)
    def update(k: Rep[K], v: Rep[V]) = map_update(m,k,v)
    def contains(k: Rep[K]) = map_contains(m, k)
    def size = map_size(m)
    def values = map_values(m)
    def clear() = map_clear(m)
    def keys = map_keys(m)
    def foreach(block: Rep[(K,V)] => Rep[Unit]) = map_foreach(m, block)
    def mapValues[U:Manifest](block: Rep[V] => Rep[U]) = map_mapValues(m, block)
    def filter(block: Rep[(K,V)] => Rep[Boolean]) = map_filter(m, block)
  }

  def map_new[K:JSKey,V:Manifest]() : Rep[Map[K,V]]
  def map_apply[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K]): Rep[V]
  def map_get[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K]): Rep[Option[V]]
  def map_update[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K], v: Rep[V]): Rep[Unit]
  def map_contains[K:JSKey,V:Manifest](m: Rep[Map[K,V]], i: Rep[K]): Rep[Boolean]
  def map_size[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Int]
  def map_values[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Array[V]]
  def map_clear[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Unit]
  def map_keys[K:JSKey,V:Manifest](m: Rep[Map[K,V]]): Rep[Array[K]]
  def map_foreach[K:JSKey,V:Manifest](m: Rep[Map[K,V]], block: Rep[(K,V)] => Rep[Unit]): Rep[Unit]
  def map_mapValues[K:JSKey,V:Manifest,U:Manifest](m: Rep[Map[K,V]], block: Rep[V] => Rep[U]): Rep[Map[K,U]]
  def map_filter[K:JSKey,V:Manifest](m: Rep[Map[K,V]], block: Rep[(K,V)] => Rep[Boolean]): Rep[Map[K,V]]
}

trait JSMapsExp extends JSMaps with EffectExp with TupleOpsExp {
  case class MapNew[K:JSKey,V:Manifest]() extends Def[Map[K,V]]
  case class MapApply[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K]) extends Def[V]
  case class MapGet[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K]) extends Def[Option[V]]
  case class MapUpdate[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K], v: Exp[V]) extends Def[Unit]
  case class MapContains[K:JSKey,V:Manifest](m: Exp[Map[K,V]], i: Exp[K]) extends Def[Boolean]
  case class MapSize[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) extends Def[Int]
  case class MapValues[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) extends Def[Array[V]]
  case class MapClear[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) extends Def[Unit]
  case class MapKeys[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) extends Def[Array[K]]
  case class MapForeach[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Sym[K], v: Sym[V], block: Block[Unit]) extends Def[Unit]
  case class MapMapValues[K:JSKey,V:Manifest,U:Manifest](m: Exp[Map[K,V]], v: Sym[V], block: Block[U]) extends Def[Map[K,U]]
  case class MapFilter[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Sym[K], v: Sym[V], block: Block[Boolean]) extends Def[Map[K,V]]

  def map_new[K:JSKey,V:Manifest]() = reflectMutable(MapNew[K,V]())
  def map_apply[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K]) = MapApply(m,k)
  def map_get[K:JSKey,V:Manifest](m: Rep[Map[K,V]], k: Rep[K]) = MapGet(m, k)
  def map_update[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K], v: Exp[V]) =
    // reflectWrite(m)(MapUpdate(m,k,v))
    reflectEffect(MapUpdate(m,k,v))
  def map_contains[K:JSKey,V:Manifest](m: Exp[Map[K,V]], i: Exp[K]) = MapContains(m, i)
  def map_size[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) = MapSize(m)
  def map_values[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) = MapValues(m)
  def map_clear[K:JSKey,V:Manifest](m: Exp[Map[K,V]]) =
    // reflectWrite(m)(MapClear(m))
    reflectEffect(MapClear(m))
  def map_keys[K:JSKey,V:Manifest](m: Rep[Map[K,V]]) = MapKeys(m)
  def map_foreach[K:JSKey,V:Manifest](m: Exp[Map[K,V]], block: Exp[(K,V)] => Exp[Unit]) = {
    val k = fresh[K]
    val v = fresh[V]
    val b = reifyEffects(block(k -> v))
    reflectEffect(MapForeach(m, k, v, b), summarizeEffects(b).star)
  }
  def map_mapValues[K:JSKey,V:Manifest,U:Manifest](m: Exp[Map[K,V]], block: Exp[V] => Exp[U]) = {
    val v = fresh[V]
    val b = reifyEffects(block(v))
    reflectEffect(MapMapValues(m, v, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def map_filter[K:JSKey,V:Manifest](m: Exp[Map[K,V]], block: Exp[(K,V)] => Exp[Boolean]) = {
    val k = fresh[K]
    val v = fresh[V]
    val b = reifyEffects(block(k -> v))
    reflectEffect(MapFilter(m, k, v, b), Alloc() andAlso summarizeEffects(b).star)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case MapForeach(m, k, v, body) => syms(m):::syms(body)
    case MapMapValues(m, v, body) => syms(m):::syms(body)
    case MapFilter(m, k, v, body) => syms(m):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MapForeach(m, k, v, body) => k :: v :: effectSyms(body)
    case MapMapValues(m, v, body) => v :: effectSyms(body)
    case MapFilter(m, k, v, body) => k :: v :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MapForeach(m, k, v, body) => freqNormal(m):::freqHot(body)
    case MapMapValues(m, v, body) => freqNormal(m):::freqHot(body)
    case MapFilter(m, k, v, body) => freqNormal(m):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

/** Generates javascript code that deals with maps.
 *  Higher order functions `mapValues`, `filter` and `foreach` are transformed
 *  to loops and function definitions.
 *
 *  @author Lucas Satabin
 */
trait JSGenMaps extends JSGenEffect with QuoteGen {
  val IR: JSMapsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MapNew()               => emitValDef(sym, "{}")
    case MapApply(m,k)          => emitValDef(sym, q"$m[$k]")
    case MapGet(m,k)            => emitValDef(sym, q"$m[$k]")
    case MapUpdate(m,k,v)       => emitValDef(sym, q"$m[$k] = $v")
    case MapContains(m,i)       => emitValDef(sym, q"$i in $m")
    case MapSize(m)             => emitValDef(sym, q"Object.keys($m).length")
    case MapValues(m)           =>
      emitValDef(sym, q"Object.keys($m).map(function(k) { return $m[k] })")
    case MapClear(m)            =>
      stream.println(q"var $sym = Object.keys($m).forEach(function(_el) {")
      stream.println(q"delete $m[_el];")
      stream.println("})")
    case MapKeys(m)             => emitValDef(sym, q"Object.keys($m)")
    case MapForeach(m, k, v, b) =>
      stream.println(q"var $sym = Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      stream.println("})")
    case MapMapValues(m, v, b)  =>
      val k = fresh[Any]
      stream.println(q"var $sym = {};")
      stream.println(q"Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      val res = getBlockResult(b)
      stream.println(q"$sym[$k] = $res;")
      stream.println("})")
    case MapFilter(m, k, v, b)  =>
      stream.println(q"var $sym = {};")
      stream.println(q"Object.keys($m).forEach(")
      stream.println(q"function($k){")
      stream.println(q"var $v = $m[$k];")
      emitBlock(b)
      val res = getBlockResult(b)
      stream.println(q"if($res) {")
      stream.println(q"$sym[$k] = $v")
      stream.println("}")
      stream.println("})")
    case _                      => super.emitNode(sym, rhs)
  }
}
