package lars.graph

import lars.graph.alg.DirectedDFS

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/TransitiveClosure.java.html
 */
case class TransitiveClosure[V](g: DiGraph[V]) {

  var tc = new collection.mutable.HashMap[V,DirectedDFS[V]]()

  for (v <- g.nodes) {
    tc(v)=DirectedDFS(g,v)
  }

  def reachable(v: V, w:V): Boolean = {
    tc(v).marked(w)
  }


}
