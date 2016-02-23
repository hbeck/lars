package lars.tms.supp

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.standard.inspect.AtAtoms
import lars.tms.status.Labels
import lars.tms.status.Status.{in, out}

/**
 * Created by hb on 7/15/15.
 */
object SuppAt {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = x match {
    case a:Atom =>
      // note the misleading "or" in the ijcai paper. we are only interested in the case x is an (ordinary) atom
      // this way of writing makes this more explicit
      if (L.status(a) == out) {
        return Set()
      }
//      val a = a.asInstanceOf[Atom]
      Set[ExtendedAtom]() ++ AtAtoms(P).filter(y => y.atom == a && L.status(y) == in)
    case _ => Set()


/*    if (!x.isInstanceOf[Atom]) {
      return Set()
    }
    // note the misleading "or" in the ijcai paper. we are only interested in the case x is an (ordinary) atom
    // this way of writing makes this more explicit
    if (L.status(x) == out) {
      return Set()
    }
    val a = x.asInstanceOf[Atom]
    Set[ExtendedAtom]() ++ AtAtoms(P).filter(y => y.atom == a && L.status(y) == in)*/
  }


}