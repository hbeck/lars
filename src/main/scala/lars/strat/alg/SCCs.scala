package lars.strat.alg

import lars.core.semantics.formulas.ExtendedAtom
import lars.strat.DepGraph

/**
 * strongly connected components
 *
 * Created by hb on 7/10/15.
 */
object SCCs extends (DepGraph => Map[ExtendedAtom,DepGraph]) {

  override def apply(g: DepGraph): Map[ExtendedAtom, DepGraph] = ???

}
