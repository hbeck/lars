package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsAt {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    x match {
      case y@AtAtom(t,a) => {
        if (P.rules.flatMap(r=> r.B + r.h).contains(y)) {
          return Set(a)
        } else {
          return Set()
        }
      }
      case _ => Set()
    }
  }

}
