package lars.tms.acons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.cons._
import lars.tms.status.Labels
import lars.tms.supp.{Supp, Support}

/**
 * Created by hb on 7/14/15.
 */
object ACons {

  def apply(P: StdProgram, L: Labels, support: Support, a: ExtendedAtom): Set[ExtendedAtom] = {
    Cons(P, a).filter(b => support.supp(b).exists(c => c.nested.contains(a))) union ConsAt(P,a)
  }

  //instead of having int l as parameter, assume strata are known outside
  //and simply provide the program P_l
  //thus, this method implements also the definition of ACons(A,t,l)
  def apply(P: StdProgram, L: Labels, A: Set[Atom], supp: Support, t: Int): Set[ExtendedAtom] = t match {
      case 0 => A.flatMap(a => ConsStar(P,a))
      case _ => A.flatMap(a => AConsStar(P,L,supp,AtAtom(t,a)) -- A)
//      case _ => A.flatMap(a => AConsStar(P,L,supp,a) -- A)
    }
}
