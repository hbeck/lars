package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.AtAtom
import lars.core.semantics.programs.general.GeneralProgram
import lars.core.semantics.programs.standard.ExtendedAtoms

/**
 * Created by hb on 7/14/15.
 */
object HeadAtAtoms {

  def apply(P: GeneralProgram): Set[AtAtom] = {
    P.rules.map(_.head).flatMap(ExtendedAtoms(_, true)).filter(_.isInstanceOf[AtAtom]).map(_.asInstanceOf[AtAtom])
  }

}
