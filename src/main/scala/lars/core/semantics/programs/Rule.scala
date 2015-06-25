package lars.core.semantics.programs

import lars.core.semantics.formulas.Formula

/**
 * Created by hb on 6/23/15.
 */
case class Rule(head:Formula, body:Formula) {
  def asFormula = body implies head

  override def equals(that: Any) = {
    that match {
      case Rule(h,b) => head == h && body == b
      case _ => false
    }
  }
}
