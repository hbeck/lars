package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.DiGraph
import lars.graph.alg.{SCCFn, DepPartition}
import lars.graph.quotient.QuotientGraph

import scala.collection.immutable.HashMap

/**
 * Created by et on 22.07.15.
 */
object  StratumGraph {

  def apply(g: DepGraph, q: QuotientGraph[ExtendedAtom]): QuotientGraph[ExtendedAtom] = {
    val result = QuotientGraph(q,DepPartition(g))
    var adjacencyList = new HashMap[Set[ExtendedAtom],Set[Set[ExtendedAtom]]]()

    for ((key, value) <- result.adjList) {
      var newV:Set[Set[ExtendedAtom]] = Set()
      for ( e <- value) {
        newV += e.flatten
      }
      adjacencyList += (key.flatten -> newV)
    }
    new QuotientGraph[ExtendedAtom](adjacencyList)
  }
}
