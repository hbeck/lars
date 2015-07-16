package lars.strat.alg

import lars.strat.DepGraph
import lars.util.graph.InOutGraph

import scala.collection.immutable.HashMap

/**
 * Buggy: Cannot deal with cycles
 * 
 * Created by hb on 7/16/15.
 */
object BuggyNumbering {
  
  //   select random node, give it index 0
  //   walk the component BFS in all directions with increasing/decreasing numbers:
  //   given the current node n has index i, assign
  //   - index i+1 for neighbors m where e(m,n), and
  //   - index i-1 for neighbors m where e(n,m)
  //
  // normalize every component s.t. lowest index in every component is 0
  def apply(g: InOutGraph[DepGraph]): Map[DepGraph,Int] = {

    var graph2stratum:Map[DepGraph,Int] = new HashMap[DepGraph,Int]

    val remNodes = collection.mutable.Set[DepGraph]()
    remNodes ++= g.nodes
    var currMinStratum = 0
    var currVisitedNodes = Set[DepGraph]()
    def walk(node: DepGraph, stratum: Int) : Unit = {
      if (remNodes.contains(node)) {
        if (stratum < currMinStratum) currMinStratum = stratum
        graph2stratum += node -> stratum
        remNodes -= node
        currVisitedNodes += node
        for (to <- g.outgoing(node)) {
          walk(to, stratum - 1)
        }
        for (from <- g.incoming(node)) {
          walk(from, stratum + 1)
        }
      }
    }

    while (!remNodes.isEmpty) {
      currMinStratum = 0
      currVisitedNodes = Set[DepGraph]()
      val startNode = remNodes.head
      walk(startNode, 0)
      //normalize
      if (currMinStratum < 0) {
        for (n <- currVisitedNodes) {
          val i = graph2stratum(n)
          graph2stratum = graph2stratum.updated(n,i+currMinStratum)
        }
      }
    }

    graph2stratum
  }

}
