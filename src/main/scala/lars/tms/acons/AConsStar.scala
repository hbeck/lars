package lars.tms.acons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.status.Labels

import scala.annotation.tailrec

/**
 * Created by hb on 7/15/15.
 */
object AConsStar {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
    _apply(P,L,Set(x))
  }

  @tailrec
  def _apply(P: StdProgram, L: Labels, xs: Set[ExtendedAtom]): Set[ExtendedAtom] = {
    val next: Set[ExtendedAtom] = xs.flatMap(ACons(P,L,_))
    if (next == xs) {
      return xs
    } else {
      return _apply(P,L,next)
    }
  }

}
