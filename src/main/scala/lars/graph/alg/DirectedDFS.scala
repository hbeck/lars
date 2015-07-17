package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/DirectedDFS.java.html
 */
case class DirectedDFS(G: DepGraph, s: ExtendedAtom) {

  var count = 0
  var marked = new collection.mutable.HashMap[ExtendedAtom,Boolean]()
  for (n <- G.nodes) {
    marked(n)=false
  }
  dfs(G,s)

  def dfs(G: DepGraph, v: ExtendedAtom) : Unit = {
    count += 1
    marked(v)=true
    for (w <- G.outgoing(v)) {
      if (!marked(w)) dfs(G,w)
    }
  }
}
