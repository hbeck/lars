package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.{Program, Rule}

/**
 * Difference to IntensionalAtoms is that this one only gives back atoms that appear unnested, i.e.,
 * not (only) within an AtAtom
 *
 * Created by hb on 7/14/15.
 */
object HeadOrdinaryAtoms {

  def apply(P: StdProgram): Set[Atom] = {
    P.rules.map(_.h).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

  def apply[R <: Rule, Pr <: Program[R]](P: Pr): Set[Atom] = {
    P.rules.map(_.head).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom])
  }

}
