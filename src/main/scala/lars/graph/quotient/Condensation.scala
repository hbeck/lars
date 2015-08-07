package lars.graph.quotient

import lars.graph.DiGraph
import lars.graph.alg.SCCFn

/**
 * the condensation of a strongly connected graph is the quotient graph where
 * the strongly connected components form the blocks of the partition.
 *
 * Created by hb on 7/10/15.
 */
object Condensation {

  def apply[V](G: DiGraph[V]): QuotientGraph[V] = {
   QuotientGraph(G,SCCFn[V]())
  }

}
