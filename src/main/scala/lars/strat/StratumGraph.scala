package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.alg.DepPartition
import lars.graph.quotient.QuotientGraph

/**
 * Created by et on 22.07.15.
 */
object StratumGraph {

  /*NOTE new quotientgraph from DepGraph*/
  def apply(g: DepGraph): QuotientGraph[ExtendedAtom]  = {
    QuotientGraph(g,DepPartition())
  }
}
