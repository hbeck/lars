package lars.core.semantics.programs.standard

import lars.core.semantics.formulas._

/**
 * Created by hb on 7/13/15.
 */
object ExtendedAtoms {

  def apply(P: StdProgram) : Set[ExtendedAtom] = {
    P.rules.flatMap( r => r.B + r.H)
  }

}
