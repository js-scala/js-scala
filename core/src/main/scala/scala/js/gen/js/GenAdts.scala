package scala.js.gen.js

import scala.js.exp.AdtsExp

trait GenAdts extends GenBase with GenFunctions {
  val IR: AdtsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case AdtApply(fields) => emitValDef(sym, literalObjectDef(fields))
    case AdtSelect(obj, label) => emitValDef(sym, literalObjectSelect(obj,label))
    case AdtEqual(obj, bis, fieldsObj, fieldsBis) => 
      if(fieldsObj.size>0){
        val fields = fieldsObj.zip(fieldsBis)
        if(fields.size==1){
          val valDef = ("(" + literalObjectSelect(obj,fields(0)._1) + "==" + literalObjectSelect(bis,fields(0)._2) + ")")
          emitValDef(sym, valDef)
        }else{
          val valDef = fields.reduceLeft{(field, n) => 
            if (field==fields(0)){
              ("(" + literalObjectSelect(obj,n._1) + "==" + literalObjectSelect(bis,n._2) + ")" + "&&" + "(" +  literalObjectSelect(obj,field._1) + "==" + literalObjectSelect(bis,field._2) + ")", "")
            }else{
              ("(" + literalObjectSelect(obj,n._1) + "==" + literalObjectSelect(bis,n._2) + ")" + "&&" + field._1, "")
            }
          }
          emitValDef(sym, valDef._1)
        }
      }else{
        emitValDef(sym, quote(obj) + "==" + quote(bis))
      }
    case AdtFold(obj, fs) => {
      emitValDef(sym, "["+fs.map(quote).mkString(",")+"]["+quote(obj)+".$variant]("+quote(obj)+")")
    }
    case _ => 
      super.emitNode(sym, rhs)
       
  }
}