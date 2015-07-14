package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.general.GeneralProgram

/**
 * Created by hb on 7/14/15.
 */
object HeadOrdinaryAtoms {

  def apply(P: GeneralProgram): Set[Atom] = {
    P.rules.map( _.head ).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

}
