package lars.tms

import lars.core.ClosedIntInterval
import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.{WDiam, AtAtom, WindowAtom}
import lars.core.semantics.streams.S
import lars.tms.status.Labels

/**
 * Created by et on 09.09.15.
 */
trait WindowAtomOperators {

  def exp(omega: WindowAtom, L:Labels, t: Int, fired: Set[(WindowAtom, Int)]): Option[ExtendedAtom]
  def aR(atom: ExtendedAtom, wa: WindowAtom, lower: Int, upper: Int): ClosedIntInterval
  def q(omega: WindowAtom, L:Labels): Set[Int]
  def SIn(wa: WindowAtom, t: Int, l: Int, D: S, tm: Set[ClosedIntInterval]): Option[ClosedIntInterval]
  def SOut(wfn: WindowAtom, t: Int): ClosedIntInterval
}
