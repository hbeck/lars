package lars.tms.supp

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.{StdRule, StdProgram}
import lars.core.semantics.programs.standard.inspect.PH
import lars.tms.status.Labels
import lars.tms.status.Status.{in, out}
import lars.tms.status.rule.fVal

/**
 * Created by hb on 7/15/15.
 */
object SuppM {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
    if (L.status(x) != out) {
      return Set()
    }
    val xRules: Set[StdRule] = PH(P,x)
    if (xRules.exists(fVal(L,_))) {
      return Set()
    }
    xRules.map(pick(L,_))
  }

  private def pick(L: Labels, r: StdRule): ExtendedAtom = {
    //TODO revisit influence of order/choice
    val optP = r.Bp.find(L.status(_) == out)
    if (optP.isDefined) {
      return optP.get
    }
    r.Bn.find(L.status(_) == in).get
  }

}