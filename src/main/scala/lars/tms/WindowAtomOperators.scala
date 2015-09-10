package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.WindowAtom
import lars.core.windowfn.WindowFunctionFixedParams
import lars.tms.status.Labels

/**
 * Created by et on 09.09.15.
 */
abstract class WindowAtomOperators {

  def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(ExtendedAtom, WindowAtom, Int)]): Option[Set[ExtendedAtom]]
  def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval
}
