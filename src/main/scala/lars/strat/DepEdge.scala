package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/17/15.
 */
case class DepEdge(from: ExtendedAtom, to: ExtendedAtom, dep: Dependency)
