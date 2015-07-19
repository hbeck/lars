package lars.tms

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 6/25/15.
 */
case class TMSModel(in:ExtendedAtom=>Boolean, out:ExtendedAtom=>Boolean) {
  def isAdmissible(tms:TMS): Boolean = {
    //TODO
    false
  }
}
