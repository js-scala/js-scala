package scala.js.exp.dom

import scala.js.language.dom.NodeListOps
import scala.virtualization.lms.common.EffectExp

trait NodeListOpsExp extends NodeListOps with EffectExp {

  def nodeList_size[A : Manifest](ns: Exp[NodeList[A]]) = {
    NodeListSize[A](ns)
  }
  
  def nodeList_filter[A : Manifest](ns: Exp[NodeList[A]],f: Exp[A] => Exp[Boolean]) = {
    val it = fresh[A]
    val block = reifyEffects(f(it))
    reflectEffect(NodeListFilter(ns, it, block), summarizeEffects(block).star)
  }
  
  def nodeList_foreach[A: Manifest](ns: Exp[NodeList[A]], f: Exp[A] => Exp[Unit]) = {
    val it = fresh[A]
    val block = reifyEffects(f(it))
    reflectEffect(NodeListForeach(ns,it,block), summarizeEffects(block).star)
  }

  def nodeList_map[A : Manifest, B : Manifest](ns: Exp[NodeList[A]], f: Exp[A] => Exp[B]) = {
    val it = fresh[A]
    val block = reifyEffects(f(it))
    reflectEffect(NodeListMap(ns, it, block), summarizeEffects(block).star)
  }

  def nodeList_foreachWithIndex[A : Manifest](ns: Exp[NodeList[A]], f: (Exp[A], Exp[Int]) => Exp[Unit]) = {
    val a = fresh[A]
    val i = fresh[Int]
    val block = reifyEffects(f(a, i))
    reflectEffect(NodeListForeachWithIndex(ns, a, i, block), summarizeEffects(block).star)
  }
  
  case class NodeListSize[A : Manifest](ns: Exp[NodeList[A]]) extends Def[Int]
  case class NodeListFilter[A : Manifest](ns: Exp[NodeList[A]], x: Sym[A], block: Block[Boolean]) extends Def[List[A]]
  case class NodeListForeach[A : Manifest](ns: Exp[NodeList[A]], x: Sym[A], block: Block[Unit]) extends Def[Unit]
  case class NodeListMap[A : Manifest, B : Manifest](ns: Exp[NodeList[A]], x: Sym[A], block: Block[B]) extends Def[List[B]]
  case class NodeListForeachWithIndex[A : Manifest](ns: Exp[NodeList[A]], a: Sym[A], i: Sym[Int], block: Block[Unit]) extends Def[Unit]

  override def syms(e: Any) = e match {
    case NodeListFilter(ns, _, block) => syms(ns) ++ syms(block)
    case NodeListForeach(ns, _, block) => syms(ns) ++ syms(block)
    case NodeListMap(ns, _, block) => syms(ns) ++ syms(block)
    case NodeListForeachWithIndex(ns, _, _, block) => syms(ns) ++ syms(block)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any) = e match {
    case NodeListFilter(_, x, block) =>  x :: effectSyms(block)
    case NodeListForeach(_, x, block) =>  x :: effectSyms(block)
    case NodeListMap(_, x, block) =>  x :: effectSyms(block)
    case NodeListForeachWithIndex(_, a, i, block) => a :: i :: effectSyms(block)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any) = e match {
    case NodeListFilter(ns, _, block) =>  freqNormal(ns) ++ freqHot(block)
    case NodeListForeach(ns, _, block) =>  freqNormal(ns) ++ freqHot(block)
    case NodeListMap(ns, _, block) =>  freqNormal(ns) ++ freqHot(block)
    case NodeListForeachWithIndex(ns, _, _, block) => freqNormal(ns) ++ freqHot(block)
    case _ => super.symsFreq(e)
  }

}