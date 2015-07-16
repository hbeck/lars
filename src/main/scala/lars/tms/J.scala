package lars.tms

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 6/25/15.
 */
case class J(I:Set[ExtendedAtom], O:Set[ExtendedAtom], n:ExtendedAtom) {

  def fires (m:TMSModel): Boolean = {
    (I forall m.in) && (O forall m.out)
  }

}
