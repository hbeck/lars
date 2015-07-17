package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph
import lars.strat.alg.TransitiveClosure

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/BruteSCC.java.html
 */
case class BruteSCC(G: DepGraph) {

  val compId = new collection.mutable.HashMap[ExtendedAtom,Int]()
  val node = new collection.mutable.HashMap[Int,ExtendedAtom]()

  private var i = -1
  for (v <- G.nodes) {
    i += 1
    compId(v)=i
    node(i)=v //(no need to keep in sync with compId later)
  }
  val N = i
   
  val tc = TransitiveClosure(G)
  
  for (v <- 0 to N) {
    for (w <- 0 to (v-1)) {
      if (tc.reachable(node(v),node(w)) &&
          tc.reachable(node(w),node(v))) {
        compId(node(v)) = compId(node(w))
      }
    }
  }
  
  private var c = 0
  for (v <- 0 to N) {
    if (compId(node(v)) == v) {
      c += 1
    }
  }

  val count = c

  def stronglyConnected(v: ExtendedAtom, w: ExtendedAtom): Boolean = {
    compId(v) == compId(w)
  }

}
