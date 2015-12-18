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
    _apply(P,L,x,x)
  }

  def _apply(P: StdProgram, L: Labels, xs: ExtendedAtom, xp: ExtendedAtom): Set[ExtendedAtom] = {
/*    val next: Set[ExtendedAtom] = xs.flatMap(ACons(P,L,_))
    if (next == xs) {
      xs
    } else {
      _apply(P,L,next)
    }*/


    val next: Set[ExtendedAtom] = exclude(ACons(P,L,xs),xs)

    val clean = exclude(next,xp)

    if (clean.nonEmpty) {
      return clean.flatMap(x => _apply(P,L,x,xs)) + xs
    }
    Set(xs)
  }

  def exclude(xs: Set[ExtendedAtom], x: ExtendedAtom): Set[ExtendedAtom] = xs.contains(x) match {
    case true => xs - x
    case _ =>xs
  }

}
