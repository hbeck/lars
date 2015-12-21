package lars.core.semantics.programs.standard

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.Program

/**
 * Program as used in Answer Update Algorithm
 *
 * Created by hb on 6/23/15.
 */
case class StdProgram(override val rules:Set[StdRule]=Set[StdRule]()) extends Program[StdRule](rules) {

  def contains(x: ExtendedAtom): Boolean = {
    rules.exists(r => r.contains(x))
//    rules.find( r => r.contains(x) ).isDefined
  }

  def +(rule:StdRule) = StdProgram(rules + rule)
  def ++(rules:Set[StdRule]) = StdProgram(this.rules ++ rules)
  def ++(other:StdProgram) = StdProgram(this.rules ++ other.rules)

  override def apply(rules:Set[StdRule]): StdProgram = StdProgram(rules)

}