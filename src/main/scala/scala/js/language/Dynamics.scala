package scala.js.language

import scala.virtualization.lms.common.Base
import scala.Dynamic

trait Dynamics extends Base {
  type DynamicRep <: DynamicRepImpl with Rep[Any]
  trait DynamicRepImpl extends Dynamic {
    def applyDynamic(field: String): ApplyDynamicSelector
    def selectDynamic(field: String): DynamicRep
    def updateDynamic(field: String)(value: Rep[Any]): Rep[Unit]
  }
  trait ApplyDynamicSelector {
    def apply(args: Rep[Any]*): DynamicRep
    // TODO: array-like update
    //def update(values: Rep[Any]*): Rep[Unit]
  }
  def dynamic(x: Rep[Any]): DynamicRep
  def newDynamic(constructor: String)(args: Rep[Any]*): DynamicRep
  def inlineDynamic(code: String): DynamicRep
}