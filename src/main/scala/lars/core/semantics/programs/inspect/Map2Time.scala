package lars.core.semantics.programs.inspect

import lars.core.semantics.formulas.{Atom, AtAtom}

/**
 * Created by hb on 7/14/15.
 */
object Map2Time {

  def apply(xs:Set[AtAtom]): Map[Atom,Set[Int]] = {
    val mMap = new collection.mutable.HashMap[Atom,Set[Int]]()
    for (x@AtAtom(t,a) <- xs) {
      if (mMap.contains(a)) {
        val s = mMap(a) + t
        mMap(a)=s
      } else {
        mMap(a)=Set(t)
      }
    }
    mMap.toMap
  }

}
