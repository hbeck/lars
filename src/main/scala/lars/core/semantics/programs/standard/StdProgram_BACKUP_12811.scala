package lars.core.semantics.programs.standard

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.Program

/**
 * Program as used in Answer Update Algorithm
 *
 * Created by hb on 6/23/15.
 */
case class StdProgram(override val rules:Set[StdRule]) extends Program[StdRule](rules) {

  def contains(x: ExtendedAtom): Boolean = {
    rules.exists(r => r.contains(x))
<<<<<<< HEAD
/*    rules.find( r => r.contains(x) ).isDefined*/
=======
//    rules.find( r => r.contains(x) ).isDefined
>>>>>>> tms
  }

  override def apply(rules:Set[StdRule]): StdProgram = StdProgram(rules)

}