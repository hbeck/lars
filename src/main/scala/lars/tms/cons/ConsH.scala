package lars.tms.cons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.{StdRule, StdProgram}

/**
 * Created by hb on 7/14/15.
 */
object ConsH {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = P.rules.filter(_.B.contains(x)).map(_.h)

}
