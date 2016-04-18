package lars.tms.acons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.cons._
import lars.tms.status.Labels
import lars.tms.supp.{SuppAt, SuppN, SuppP, Supp}

/**
 * Created by hb on 7/14/15.
 */
object ACons {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
/*    println("X: "+x)
    println("Cons: "+Cons(P,x))
    Cons(P,x).foreach(a => println("Supp("+a+"): "+Supp(P,L,a)))
    println("ConsAt("+x+"): "+ConsAt(P,x))
    println("Acons for "+x+": "+(Cons(P, x).filter(y => !Supp(P, L, y.atom).contains(x)) union ConsAt(P, x)))*/
    Cons(P, x).filter(y => !Supp(P, L, y.atom).contains(x)) union ConsAt(P, x)
  }

  //instead of having int l as parameter, assume strata are known outside
  //and simply provide the program P_l
  //thus, this method implements also the definition of ACons(A,t,l)
  def apply(P: StdProgram, L: Labels, A: Set[Atom], t: Int): Set[ExtendedAtom] = t match {
      case 0 => A.flatMap(a => ConsStar(P,a))
      case _ => A.flatMap(a => AConsStar(P,L,AtAtom(t,a)) -- A)
//      case _ => A.flatMap(a => AConsStar(P,L,a) -- A)
    }
}
