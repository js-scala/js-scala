package scala.js.gen.js

import java.io.PrintWriter

import scala.virtualization.lms.internal.GenericNestedCodegen

trait NestedCodegenOpt extends GenericNestedCodegen {
  import IR._

  val defUseMap : scala.collection.mutable.Map[Sym[_], Int] = new scala.collection.mutable.HashMap
  def addDefUse(sym: Sym[_]) = defUseMap.update(sym, defUseMap.getOrElse(sym, 0) + 1)
  def defUse(sym: Sym[_]) = defUseMap.getOrElse(sym, 0)

  def buildDefUse(body: Block[Any]): Unit = {
    defUseMap.clear()
    
    refSyms(getBlockResult(body)).foreach(addDefUse)
    for (TP(_, rhs) <- buildScheduleForResult(body, false /*unsorted*/)) {
      for (sym <- refSyms(rhs)) {
	addDefUse(sym)
      }
    }
  }

  def refSyms(e: Any): List[Sym[Any]] = e match {
    case Reify(x, u, es) => readSyms(x)
    case _ => readSyms(e)
  }

  abstract override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], methName: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    buildDefUse(body)
    super.emitSource(args, body, methName, stream)
  }

  abstract override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (defUse(sym) == 0) stream.println(rhs)
    else super.emitValDef(sym, rhs)
  }
}

trait CodegenOpt extends NestedCodegen with NestedCodegenOpt