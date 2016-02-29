package lars.tms.supp

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram
import lars.core.semantics.programs.standard.inspect.PH
import lars.tms.status.Labels
import lars.tms.status.Status.in
import lars.tms.status.rule.fVal

/**
 * Created by hb on 7/15/15.
 */
object SuppP {

  def apply(P: StdProgram, L: Labels, x: ExtendedAtom): Set[ExtendedAtom] = {
    println("status("+x+"): "+L.status(x))
    if (L.status(x) != in) {
      return Set()
    }
    PH(P,x).foreach(c => println("fval("+c+": "+fVal(L,c)))
    PH(P,x).filter(fVal(L,_)).flatMap(_.B)
  }

}