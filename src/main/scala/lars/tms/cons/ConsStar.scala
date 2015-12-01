package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram

import scala.annotation.tailrec

/**
 * Created by hb on 7/14/15.
 */
object ConsStar {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    _apply(P, x, x) - x
  }

  def _apply(P: StdProgram, xs: ExtendedAtom, xp:ExtendedAtom): Set[ExtendedAtom] = {
    val next: Set[ExtendedAtom] = exclude(Cons(P,xs),xs)

    val clean = exclude(next,xp)

    if (clean.nonEmpty) {
      return clean.flatMap(x => _apply(P,x,xs)) + xs
    }
    Set(xs)
  }

  def exclude(xs: Set[ExtendedAtom], x: ExtendedAtom): Set[ExtendedAtom] = xs.contains(x) match {
    case true => xs - x
    case _ =>xs
  }

}


