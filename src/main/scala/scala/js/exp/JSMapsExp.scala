package scala.js.exp

import scala.js._
import scala.js.language.JSMaps
import scala.virtualization.lms.common._

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
  def map_get[K:JSKey,V:Manifest](m: Exp[Map[K,V]], k: Exp[K]) = MapGet(m, k)
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
  def map_foreach[K:JSKey,V:Manifest](m: Exp[Map[K,V]], block: ((Exp[K], Exp[V])) => Exp[Unit]) = {
    val k = fresh[K]
    val v = fresh[V]
    val b = reifyEffects(block(k, v))
    reflectEffect(MapForeach(m, k, v, b), summarizeEffects(b).star)
  }
  def map_mapValues[K:JSKey,V:Manifest,U:Manifest](m: Exp[Map[K,V]], block: Exp[V] => Exp[U]) = {
    val v = fresh[V]
    val b = reifyEffects(block(v))
    reflectEffect(MapMapValues(m, v, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def map_filter[K:JSKey,V:Manifest](m: Exp[Map[K,V]], block: ((Exp[K], Exp[V])) => Exp[Boolean]) = {
    val k = fresh[K]
    val v = fresh[V]
    val b = reifyEffects(block(k, v))
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