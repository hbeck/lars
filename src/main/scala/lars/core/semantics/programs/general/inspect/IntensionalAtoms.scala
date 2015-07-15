package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.extatoms.ExtendedAtoms
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.{Rule, Program}

/**
 * Difference to HeadOrdinaryAtoms is that this one looks also into AtAtoms
 *
 * Created by hb on 7/14/15.
 */
object IntensionalAtoms {

  def apply(P: StdProgram): Set[Atom] = {
    P.rules.map(_.h).flatMap(ExtendedAtoms(_,true)).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

  def apply[R <: Rule](P: Program[R]): Set[Atom] = {
    P.rules.map(_.head).flatMap(ExtendedAtoms(_, true)).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

}
