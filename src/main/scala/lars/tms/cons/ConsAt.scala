package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.extatoms.AtAtom
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object ConsAt {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = {
    x match {
      case AtAtom(t,a) => {
        val opt = P.rules.flatMap(_.B).find({
          case AtAtom(u,b) => (a == b) //TODO directly?
          case _ => false})
        if (opt.isDefined) {
          Set(opt.get)
        } else {
          Set()
        }
      }
      case _ => Set()
    }
  }

}
