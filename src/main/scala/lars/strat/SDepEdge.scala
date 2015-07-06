package lars.strat

import lars.core.semantics.formulas.ExtendedAtom

/**
 * Created by hb on 7/6/15.
 */
case class SDepEdge(from:ExtendedAtom, to:ExtendedAtom, dep: SDep)

sealed class SDep
case object eql extends SDep
case object grt extends SDep
case object geq extends SDep