package lars.strat

import lars.core.semantics.formulas.ExtendedAtom
import lars.graph.alg.DepPartition
import lars.graph.quotient.QuotientGraph

/**
 * Created by et on 22.07.15.
 */
object StratumGraph {

  def apply(g: DepGraph): QuotientGraph[ExtendedAtom] = {
    return QuotientGraph(g,DepPartition())
  }
}
