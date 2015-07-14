package lars.core.semantics.programs.standard

import lars.core.semantics.programs.Program

/**
  * Created by hb on 6/23/15.
  */
case class StdProgram(override val rules:Set[StdRule]) extends Program[StdRule](rules)
