package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/6/15.
 */
case class DepEdge(from:ExtendedAtom, to:ExtendedAtom, dep: Dep) {
  override def toString = from + " {" + dep + "} " + to
}

sealed abstract class Dep

case object eql extends Dep {
  override def toString = "="
}
case object grt extends Dep {
  override def toString = ">"
}
case object geq extends Dep {
  override def toString = ">="
}
