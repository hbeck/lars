package lars.tms.supp

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.status.Labels

/**
 * Created by hb on 7/14/15.
 */
object Supp {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
    SuppP(P,L,x) union SuppN(P,L,x) union SuppAt(P,L,x)
  }

}
