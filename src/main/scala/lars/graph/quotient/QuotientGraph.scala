package lars.graph.quotient

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.InOutGraph
import lars.strat.DepGraph

/**
 * https://en.wikipedia.org/wiki/Quotient_graph
 *
 * Created by hb on 7/16/15.
 */
case class QuotientGraph(override val adjList:Map[DepGraph,Set[DepGraph]]) extends InOutGraph[DepGraph](adjList)

object QuotientGraph {

  def apply(depGraph:DepGraph, sccs: collection.immutable.Map[ExtendedAtom,DepGraph]): QuotientGraph = {
    var nodes = Set[DepGraph]()
    var adjList = Map[DepGraph,Set[DepGraph]]()

    //TODO

    new QuotientGraph(adjList)
  }
}
