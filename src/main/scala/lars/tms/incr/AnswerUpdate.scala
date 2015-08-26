package lars.tms.incr

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.WindowAtom
import lars.core.semantics.programs.general.inspect.ExtensionalAtoms
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.streams.{Timeline, S}
import lars.strat.Strata
import lars.tms.TMS
import lars.tms.cons.ConsStar
import lars.tms.incr.Result.{fail, success}
import lars.tms.status.Status.{unknown, out}
import lars.tms.status.{Label, Labels}

/**
 * Created by hb on 7/16/15.
 */
class AnswerUpdate(P: StdProgram) {

  private val tms:TMS = TMS(P)
  //

}
