package scala.js.gen.js

import scala.js.exp.RecordsExp

trait GenRecords extends GenBase {
  val IR: RecordsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case RecordApply(fields) => emitValDef(sym, literalObjectDef(fields))
    case RecordSelect(obj, label) => emitValDef(sym, literalObjectSelect(obj,label))
    case RecordEqual(obj, bis, fieldsObj, fieldsBis) => 
      if(fieldsObj.size>0){
        val fields = fieldsObj.zip(fieldsBis)
        val x = fresh[Boolean]
        emitValDef(x, literalObjectSelect(obj,fieldsObj(0)) + "==" + literalObjectSelect(bis,fieldsBis(0)))
        emitValDef(sym, quote(x) + "&&" + equalRec(obj, bis, fields, 1))
      }else{
        
      }
      
    case _ => super.emitNode(sym, rhs)
  }
  
  def equalRec[A](obj: Exp[A], bis: Exp[A], fields: Seq[(String, String)], index: Int): String = {
    
    if(index == fields.size-1){
       val x = fresh[Boolean]
       emitValDef(x, literalObjectSelect(obj,fields(index)._1) + "==" + literalObjectSelect(bis,fields(index)._2))
       quote(x)
    }else{
      val x, y = fresh[Boolean]
      emitValDef(x, literalObjectSelect(obj,fields(index)._1) + "==" + literalObjectSelect(bis,fields(index)._2))
      emitValDef(y, quote(x) + "&&" + equalRec(obj, bis, fields, index+1))
      quote(y)
    }
  }
}