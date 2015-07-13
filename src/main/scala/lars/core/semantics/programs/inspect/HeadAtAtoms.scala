package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.AtAtom
import lars.core.semantics.programs.Program
import lars.strat.ExtendedAtoms

/**
 * Created by hb on 7/14/15.
 */
object HeadAtAtoms {

  def apply(P: Program): Set[AtAtom] = {
    P.rules.map(_.head).flatMap(ExtendedAtoms(_, true)).filter(_.isInstanceOf[AtAtom]).map(_.asInstanceOf[AtAtom])
  }

}
