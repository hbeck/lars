package lars.tms

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 6/25/15.
 */
case class TMS(N:Set[ExtendedAtom],J:Set[J]) {

}

object TMS {
  def apply(P: StdProgram): TMS = {
    //TODO
    TMS(Set(),Set())
  }
}
