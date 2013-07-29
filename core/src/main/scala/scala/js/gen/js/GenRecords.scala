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
        val valDef = fields.reduceLeft{(field, n) => 
          if (field==fields(0)){
            ("(" + literalObjectSelect(obj,n._1) + "==" + literalObjectSelect(bis,n._2) + ")" + "&&" + "(" +  literalObjectSelect(obj,field._1) + "==" + literalObjectSelect(bis,field._2) + ")", "")
          }else{
            ("(" + literalObjectSelect(obj,n._1) + "==" + literalObjectSelect(bis,n._2) + ")" + "&&" + field._1, "")
          }
        }
        emitValDef(sym, valDef._1)
      }else{
        emitValDef(sym, quote(obj) + "==" + quote(bis))
      }
    case _ => 
      super.emitNode(sym, rhs)
       
  }
}