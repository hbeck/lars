package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.Program
import lars.strat.ExtendedAtoms

/**
 * Created by hb on 7/14/15.
 */
object IntensionalAtoms {

  def apply(P: Program): Set[Atom] = {
    P.rules.map(_.head).flatMap(ExtendedAtoms(_, true)).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

}
