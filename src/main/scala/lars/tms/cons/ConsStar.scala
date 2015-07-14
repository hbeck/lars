package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram

import scala.annotation.tailrec

/**
 * Created by hb on 7/14/15.
 */
object ConsStar {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    _apply(P,Set(x))
  }

  @tailrec
  def _apply(P: StdProgram, xs: Set[ExtendedAtom]): Set[ExtendedAtom] = {
    val next: Set[ExtendedAtom] = xs.flatMap(Cons(P,_))
    if (next == xs) {
      return xs
    } else {
      return _apply(P,next)
    }
  }

}
