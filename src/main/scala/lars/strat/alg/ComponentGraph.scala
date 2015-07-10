package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph

import scala.collection.mutable.{HashMap, Map, Set}
import scala.math.{abs,max}


/**
 * Created by hb on 7/10/15.
 */
class ComponentGraph(val nodes:collection.immutable.Set[DepGraph]) {

  val adjList:Map[DepGraph,Set[DepGraph]] = new HashMap[DepGraph,Set[DepGraph]]
  for (n <- nodes) {
    adjList += (n -> Set[DepGraph]())
  }

  def hasEdge(n:DepGraph, m:DepGraph) : Boolean = {
    adjList(n).contains(m)
  }

  private def add(from:DepGraph,to:DepGraph): Unit = {
    adjList(from) += to
  }

  val graph2stratum:Map[DepGraph,Int] = new HashMap[DepGraph,Int]
  //   select random node, give it index 0
  //   walk the component BFS in all directions with increasing/decreasing numbers:
  //   given the current node n has index i, assign
  //   - index i+1 for neighbors m where e(m,n), and
  //   - index i-1 for neighbors m where e(n,m)
  //
  // normalize every component s.t. lowest index in every component is 0
  def assignStratumNumbers() : Unit = {

    //initialize ds:
    val out:Map[DepGraph,Set[DepGraph]] = new HashMap[DepGraph,Set[DepGraph]] //new copy of adjList
    val in:Map[DepGraph,Set[DepGraph]] = new HashMap[DepGraph,Set[DepGraph]]

    for (n <- nodes) {
      in += (n -> Set[DepGraph]())
      out += (n -> Set[DepGraph]())
    }
    for (from <- nodes) {
      val outgoing = adjList(from)
      out(from) ++= outgoing
      for (to <- outgoing) {
        in(to) += from
      }
    }

    //alg:
    val remNodes = Set[DepGraph]()
    remNodes ++= nodes
    var currMinStratum = 0
    var currVisitedNodes = Set[DepGraph]()
    def walk(node: DepGraph, stratum: Int) : Unit = {
      if (remNodes.contains(node)) {
        if (stratum < currMinStratum) currMinStratum = stratum
        graph2stratum += node -> stratum
        remNodes -= node
        currVisitedNodes += node
        for (to <- out(node)) {
          walk(to, stratum - 1)
        }
        for (from <- in(node)) {
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
        addToStratumIdx(currVisitedNodes,abs(currMinStratum))
      }
    }

  }

  private def addToStratumIdx(nodes: Set[DepGraph], k:Int) : Unit = {
    for (n <- nodes) {
      val i = graph2stratum(n)
      graph2stratum.update(n,i+k)
    }
  }

  def maxStratum() = graph2stratum.values.reduce(max)
}

object ComponentGraph {

  // connect two components fromC and toC with an arc (in this direction),
  // if there is an edge (from,to) in g, where
  // from is in SCC fromC and to is in a different SCC toC
  // thus, ignore a further distinction between >= (geq) and > (grt) towards
  // a 'maximal' stratification (using longest possible paths)
  // alg:
  //   for all edges (from,to) in g:
  //     get SCCs fromC, toC
  //     if fromC == toC continue //dep in edge can be >= or =
  //     if there exists already an edge between fromC, toC continue
  //     if e(from,to,dep) (dep in {>=,>})
  //       create edge (fromC,toC)
  //
  // result: DAG, not necessarily (weakly) connected
  def apply(depGraph:DepGraph, sccs: collection.immutable.Map[ExtendedAtom,DepGraph]) : ComponentGraph = {
    val cg = new ComponentGraph(sccs.values.toSet)
    for (e <- depGraph.edges) {
      val fromC = sccs(e.from)
      val toC = sccs(e.to)
      if (fromC != toC && !cg.hasEdge(fromC,toC)) {
        cg.add(fromC,toC)
      }
    }
    cg
  }


}
