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

  def |=(r: Rule): Boolean = {
    !(this |= r.body) || (this |= r.head)
  }

  def |=(p: Program): Boolean = {
    p.rules.forall(this |= _)
  }

}
