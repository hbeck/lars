package lars.tms.acons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.tms.cons.{ConsStar, ConsAt, Cons}
import lars.tms.status.Labels
import lars.tms.supp.Supp

/**
 * Created by hb on 7/14/15.
 */
object ACons {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
    println("Acons cons: "+Cons(P, x))
    /*println("Acons x: "+x)
    var t:ExtendedAtom = x
    if(Cons(P, x).nonEmpty) t = Cons(P, x).head
    println("Acons label acons "+t+": "+L.label(t))
    println("Acons supp: "+Supp(P,L,t))
    println("Acons: "+(Cons(P, x).filter(y => Supp(P, L, y).contains(x)) union ConsAt(P, x)))
    Cons(P, x).foreach(y => println("supp("+y+"): "+Supp(P, L, y)))
    println("consat: "+ConsAt(P, x))*/
    Cons(P, x).filter(y => Supp(P, L, y).contains(x)) union ConsAt(P, x)
  }

  //instead of having int l as parameter, assume strata are known outside
  //and simply provide the program P_l
  //thus, this method implements also the definition of ACons(A,t,l)
  def apply(P: StdProgram, L: Labels, A: Set[Atom], t: Int): Set[ExtendedAtom] = t match {
      case _ => A.flatMap(a => ConsStar(P,a))
//      case _ => A.flatMap(a => AConsStar(P,L,AtAtom(t,a)) -- A)
    }
}
