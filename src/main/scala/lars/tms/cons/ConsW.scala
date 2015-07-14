package lars.tms.cons

import lars.core.semantics.formulas.{Atom, ExtendedAtom}
import lars.core.semantics.programs.extatoms.{WAtAtom, AtAtom, WBoxAtom, WDiamAtom}
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsW {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    x match {
      case a:Atom => P.rules.flatMap(_.B).filter({
        case WDiamAtom(_,b) => a == b //TODO (_,a) directly?
        case WBoxAtom(_,b) => a == b
        case _ => false})
      case AtAtom(t,b) => P.rules.flatMap(_.B).filter({
        case WAtAtom(_,u,a) => (t == u) && (b == a) //TODO directly?
        case _ => false})
      case _ => Set()
    }
  }

}
