package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/6/15.
 */
case class DepEdge(from:ExtendedAtom, to:ExtendedAtom, dep: Dep) {

  def reverse(): DepEdge = DepEdge(to,from,dep.inverse)

  override def toString = from + " {" + dep + "} " + to
}

sealed abstract class Dep {
  def inverse():Dep
}

case object eql extends Dep {
  override def inverse = eql
  override def toString = "="
}
case object grt extends Dep {
  override def inverse = lwr
  override def toString = ">"
}

case object geq extends Dep {
  override def inverse = leq
  override def toString = ">="
}

//for the semantic consistency of the reverse graph:
case object lwr extends Dep {
  override def inverse = grt
  override def toString = "<"
}

case object leq extends Dep {
  override def inverse = geq
  override def toString = "<="
}