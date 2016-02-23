package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsAt {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = x match{
      case y:AtAtom => {
//        if (P.rules.flatMap(r => r.B + r.h).exists(p => p.nested.contains(y))) {
        if (P.rules.flatMap(r => r.B + r.h).contains(y)) {
          Set(y.a)
        } else {
          Set()
        }
      }
      case _ => Set()
  }

}
