package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/6/15.
 */
case class DepEdge(from:ExtendedAtom, to:ExtendedAtom, dep: SDep) {
  override def toString = from + " {" + dep + "} " + to
}

sealed class SDep
case object eql extends SDep {
  override def toString = "="
}
case object grt extends SDep {
  override def toString = ">"
}

case object geq extends SDep {
  override def toString = ">="
}