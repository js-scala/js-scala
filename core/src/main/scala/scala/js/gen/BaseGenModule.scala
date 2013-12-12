package scala.js.gen

import virtualization.lms.internal.GenericCodegen

trait BaseGenModule extends GenericCodegen {
  def module: Module
  abstract sealed class Element
  class Module(val nes: Seq[(String, Element)]) extends Element
  case class Function[A : Manifest](args: List[IR.Sym[_]], body: Block[A]) extends Element {
    val Manifest = manifest[A]
  }

  def Module (nes: (String,Element)*) = new Module(nes)

  def fun[T : Manifest, R : Manifest](f: (IR.Exp[T] => IR.Exp[R])): Function[R] = {
    val s = IR.fresh[T]
    val body = reifyBlock(f(s))
    val args = List(s)
    Function(args, body)
  }
  def fun[T1 : Manifest, T2 : Manifest, R : Manifest](f: ((IR.Exp[T1], IR.Exp[T2]) => IR.Exp[R])): Function[R] = {
    val s1 = IR.fresh[T1]
    val s2 = IR.fresh[T2]
    val body = reifyBlock(f(s1, s2))
    val args = List(s1, s2)
    Function(args, body)
  }
  def fun[T1 : Manifest, T2 : Manifest, T3 : Manifest, R : Manifest](f: ((IR.Exp[T1], IR.Exp[T2], IR.Exp[T3]) => IR.Exp[R])): Function[R] = {
    val s1 = IR.fresh[T1]
    val s2 = IR.fresh[T2]
    val s3 = IR.fresh[T3]
    val body = reifyBlock(f(s1, s2, s3))
    val args = List(s1, s2, s3)
    Function(args, body)
  }
  def fun[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, R : Manifest](f: ((IR.Exp[T1], IR.Exp[T2], IR.Exp[T3], IR.Exp[T4]) => IR.Exp[R])): Function[R] = {
    val s1 = IR.fresh[T1]
    val s2 = IR.fresh[T2]
    val s3 = IR.fresh[T3]
    val s4 = IR.fresh[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    val args = List(s1, s2, s3, s4)
    Function(args, body)
  }

}