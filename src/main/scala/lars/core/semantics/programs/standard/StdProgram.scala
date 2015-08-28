package lars.core.semantics.programs.standard

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program

/**
 * Program as used in Answer Update Algorithm
 *
 * Created by hb on 6/23/15.
 */
case class StdProgram(override val rules:Set[StdRule]) extends Program[StdRule](rules) {

  def contains(x: ExtendedAtom): Boolean = {
    rules.find( r => r.contains(x) ).isDefined
  }

  override def apply(rules:Set[StdRule]): StdProgram = StdProgram(rules)

  def +(rule:StdRule): StdProgram = StdProgram(rules + rule)

  def ++(other:StdProgram): StdProgram = StdProgram(rules ++ other.rules)

}