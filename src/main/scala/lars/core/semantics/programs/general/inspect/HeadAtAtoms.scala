package lars.core.semantics.programs.general.inspect

import lars.core.semantics.programs.extatoms.{AtAtom, ExtendedAtoms}
import lars.core.semantics.programs.{Program, Rule}

/**
 * Created by hb on 7/14/15.
 */
object HeadAtAtoms {

//  def apply(P: StdProgram): Set[AtAtom] = {
//    P.rules.map(_.h).filter(_.isInstanceOf[AtAtom]).map(_.asInstanceOf[AtAtom])
//  }

  def apply[R <: Rule](P: Program[R]): Set[AtAtom] = {
    P.rules.map(_.head).flatMap(ExtendedAtoms(_, true)).filter(_.isInstanceOf[AtAtom]).map(_.asInstanceOf[AtAtom])
  }

}
