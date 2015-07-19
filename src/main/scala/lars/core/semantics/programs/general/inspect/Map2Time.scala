package lars.core.semantics.programs.general.inspect

import lars.core.semantics.formulas.Atom
import lars.core.semantics.programs.extatoms.AtAtom

/**
 * Created by hb on 7/14/15.
 */
object Map2Time {

  def apply(atAtoms:Set[AtAtom]): Map[Atom,Set[Int]] = {
    val mMap = new collection.mutable.HashMap[Atom,Set[Int]]()
    for (atAtom <- atAtoms) {
      if (mMap.contains(atAtom.a)) {
        val s = mMap(atAtom.a) + atAtom.t
        mMap(atAtom.a)=s
      } else {
        mMap(atAtom.a)=Set(atAtom.t)
      }
    }
    mMap.toMap
  }

}
