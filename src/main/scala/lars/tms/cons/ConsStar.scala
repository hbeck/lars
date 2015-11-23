package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram

import scala.annotation.tailrec

/**
 * Created by hb on 7/14/15.
 */
object ConsStar {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    _apply(P,Set(x)/*,Set()*/)
  }

  def _apply(P: StdProgram, xs: Set[ExtendedAtom]/*, xp: Set[ExtendedAtom]*/): Set[ExtendedAtom] = {
    val next: Set[ExtendedAtom] = xs.flatMap(x => exclude(Cons(P, x),x))

    if (next.isEmpty){
      xs
    } else {
     xs union  _apply(P, next)
    }


/*    if (next == xs) {
      xs
      /*what happens if next is equal to the xs of the previous iteration and next of the next iteration is equal to this xs?*/
    } else if(xp.nonEmpty && xp == next) {
      Set()
    } else {
      _apply(P,next,xs)
    }*/
  }

  def exclude(xs: Set[ExtendedAtom], x: ExtendedAtom): Set[ExtendedAtom] = xs.contains(x) match {
    case true => xs - x
    case _ => xs
  }

}


