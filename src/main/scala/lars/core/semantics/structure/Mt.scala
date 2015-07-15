package lars.core.semantics.structure

import lars.core.semantics.formulas.Formula
import lars.core.semantics.programs.{Program, Rule}
import lars.core.semantics.streams.S

/**
 * Created by hb on 5/26/15.
 */
//M,t
case class Mt(m: M, t: Int) {
  def |=(fm: Formula): Boolean = {
    val s = S(m.T, m.v)
    val mst = MSt(m, s, t)
    mst ||- fm
  }

  def |=[R <: Rule](r: R): Boolean = {
    !(this |= r.body) || (this |= r.head)
  }

  def |=[R <: Rule, Pr <: Program[R]](P: Pr): Boolean = {
    P.rules.forall(this |= _)
  }

}
