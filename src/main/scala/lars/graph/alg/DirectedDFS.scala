package lars.graph.alg

import lars.graph.DiGraph

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/DirectedDFS.java.html
 */
case class DirectedDFS[V](g: DiGraph[V], s: V) {

  var count = 0
  var marked = new collection.mutable.HashMap[V,Boolean]()
  for (n <- g.nodes) {
    marked(n)=false
  }
  dfs(g,s)

  def dfs(g: DiGraph[V], v: V) : Unit = {
    count += 1
    marked(v)=true
    for (w <- g.outgoing(v)) {
      if (!marked(w)) dfs(g,w)
    }
  }


}
