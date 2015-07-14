package lars.core.semantics.programs.general

import lars.core.semantics.formulas.Formula
import lars.core.semantics.programs.Rule

/**
 * Created by hb on 6/23/15.
 */
case class GeneralRule(head:Formula, body:Formula) extends Rule {

  override def equals(that: Any) = {
    that match {
      case GeneralRule(h,b) => head == h && body == b
      case _ => false
    }
  }

  override def toString = {
    head + " ← " + body
  }
}
