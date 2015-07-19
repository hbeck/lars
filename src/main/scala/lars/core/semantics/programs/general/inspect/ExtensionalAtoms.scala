package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.extatoms.ExtendedAtoms
import lars.core.semantics.programs.{Program, Rule}

/**
 * a.k.a input atoms
 *
 * Created by hb on 7/14/15.
 */
object ExtensionalAtoms {

  def apply[R <: Rule](P: Program[R]): Set[Atom] = {
    ExtendedAtoms(P.rules,true).filter(_.isInstanceOf[Atom]).map(_.asInstanceOf[Atom]) -- IntensionalAtoms(P)
  }

}
