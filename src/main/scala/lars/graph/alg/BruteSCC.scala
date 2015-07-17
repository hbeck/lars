package lars.graph.alg

import lars.graph.{DiGraph, TransitiveClosure}

/**
 * Created by hb on 7/11/15.
 * see http://algs4.cs.princeton.edu/42directed/BruteSCC.java.html
 */
case class BruteSCC[V](g: DiGraph[V]) {

  val compId = new collection.mutable.HashMap[V,Int]()
  val node = new collection.mutable.HashMap[Int,V]()

  private var i = -1
  for (v <- g.nodes) {
    i += 1
    compId(v)=i
    node(i)=v //(no need to keep in sync with compId later)
  }
  val N = i
   
  val tc = TransitiveClosure[V](g)
  
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

  def stronglyConnected(v: V, w: V): Boolean = {
    compId(v) == compId(w)
  }

}
