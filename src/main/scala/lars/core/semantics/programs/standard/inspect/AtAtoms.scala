package lars.core.semantics.programs.standard.inspect

import lars.core.semantics.programs.extatoms.{AtAtom, ExtendedAtoms}
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/15/15.
 */
object AtAtoms {

  /**
   * @return set of AtAtoms occurring nested/nonnested in the program
   */
  def apply(P: StdProgram, nested: Boolean=true): Set[AtAtom] = {
    ExtendedAtoms(P,nested).filter(_.isInstanceOf[AtAtom]).map(_.asInstanceOf[AtAtom])
  }

}
