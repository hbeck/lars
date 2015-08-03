package lars.graph.quotient

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.DiGraph
import lars.graph.alg.{DepPartition, SCCFn}
import lars.strat.DepGraph

/**
 * the condensation of a strongly connected graph is the quotient graph where
 * the strongly connected components form the blocks of the partition.
 *
 * Created by hb on 7/10/15.
 */
object Condensation {

  def apply[V](g: DiGraph[V]): QuotientGraph[V] = {
   QuotientGraph(g,SCCFn[V]())
  }

  def apply(g: DepGraph): QuotientGraph[ExtendedAtom] = {
    QuotientGraph(g,DepPartition())
  }

}
