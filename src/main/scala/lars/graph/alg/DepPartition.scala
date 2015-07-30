package lars.graph.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.{LabeledDiGraph, DiGraph}
import lars.strat.{Dependency, grt, DepGraph}

/**
 * Created by et on 26.07.15.
 */
case class DepPartition() extends (DepGraph => Map[ExtendedAtom,Set[ExtendedAtom]]) {

  var greater = false

  /*Currently a set is created for each node and filled.
  * Need to either create only as much sets as need, or reduce the number of sets, in such a way,
  * that only the largest subsets remain, don't overlap and don't have any grt <-> geq cycles (or contain grt edges)*/
  def apply(g: DepGraph): Map[ExtendedAtom, Set[ExtendedAtom]] = {
   // println(g.adjList)

    var r = new collection.mutable.HashMap[ExtendedAtom, Set[ExtendedAtom]]()
    var min = false
    // var s:Set[ExtendedAtom] = Set(g.nodes.head)
    for (v <- g.nodes) {
      r += (v -> Set(v))
      for (w <- g.adjList(v)) {
        while (!initDfs(g, v, w) && !r(v).contains(w)) {
          //  s += w
       //   r += (v -> Set(v))
          r(v) += w
          println(r)
        }
      }
    }

    r.toMap
  }

    def isGrt(g: DepGraph, from: ExtendedAtom, to: ExtendedAtom): Boolean = {
      g.label(from, to) match {
        case `grt` => true
        case _ => false
      }
    }

    def initDfs(g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom): Boolean = {
      greater = false
      val marked = new collection.mutable.HashMap[ExtendedAtom, Boolean]()
      for (n <- g.nodes) {
        marked(n) = false
      }
      val ret = dfs(g, v1, v2, marked)
/*      println("from: " + v1 + " to: "+ v2)
      println(greater)*/
      ret
    }

    /* @return true, if there is a ">" dependency on the path from v1 to v2, false otherwise */
    def dfs(g: DepGraph, v1: ExtendedAtom, v2: ExtendedAtom, marked: collection.mutable.HashMap[ExtendedAtom, Boolean]): Boolean = {
      marked(v1) = true
      for (w <- g.outgoing(v1)) {
        if (isGrt(g, v1, w))  greater = true
        if (w == v2)          return greater
        if (!marked(w))       dfs(g, w, v2, marked)
      }
      false
    }
}
