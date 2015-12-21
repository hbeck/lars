package lars.core.semantics.programs.standard.inspect

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.{StdRule, StdProgram}

/**
 * alpha-rules, i.e., set of rules in the program that have a given extended atom as head
 *
 * Created by hb on 7/15/15.
 */
object PH {

  def apply(P: StdProgram, x: ExtendedAtom): Set[StdRule] = {
    P.rules.filter(_.h == x)
  }

}
