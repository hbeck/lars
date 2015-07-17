package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph
import lars.util.graph.DirectedDFS

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/TransitiveClosure.java.html
 */
case class TransitiveClosure(G: DepGraph) {

  var tc = new collection.mutable.HashMap[ExtendedAtom,DirectedDFS]()
  for (v <- G.nodes) {
    tc(v)=DirectedDFS(G,v)
  }

  def reachable(v: ExtendedAtom, w:ExtendedAtom): Boolean = {
    tc(v).marked(w)
  }


}
