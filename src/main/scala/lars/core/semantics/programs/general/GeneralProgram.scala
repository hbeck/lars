package lars.core.semantics.programs.general

import lars.core.semantics.programs.Program

/**
 * Created by hb on 6/23/15.
 */
case class GeneralProgram(override val rules:Set[GeneralRule]) extends Program[GeneralRule](rules)
