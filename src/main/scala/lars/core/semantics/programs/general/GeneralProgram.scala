package lars.core.semantics.programs.general

import lars.core.semantics.programs.Program

/**
 * General LARS Program with arbitrary formulas in head and body of the rule (=GeneralRule)
  *
 * Created by hb on 6/23/15.
 */
case class GeneralProgram(override val rules:Set[GeneralRule]) extends Program[GeneralRule](rules) {
  override def apply(rules:Set[GeneralRule]): GeneralProgram = GeneralProgram(rules)
}
