package lars.tms.cons

import lars.core.semantics.formulas.ExtendedAtom
import lars.core.semantics.programs.standard.StdProgram

/**
 * Created by hb on 7/14/15.
 */
object Cons {

  def apply(P: StdProgram, x: ExtendedAtom): Set[ExtendedAtom] = ConsH(P,x) union ConsW(P,x) union ConsAt(P,x)

}
