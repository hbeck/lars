package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/11/15.
 */
package object alg {

  def e(from:ExtendedAtom, to:ExtendedAtom, dep: Dep) = DepEdge(from,to,dep)
  def g(s:Set[ExtendedAtom], e:DepEdge*): DepGraph = DepGraph(s,e.toSet)

}
