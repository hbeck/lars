package lars.tms.status.rule

import lars.core.semantics.programs.standard.StdRule
import lars.tms.status.Labels
import lars.tms.status.Status.{in, out, unknown}

/**
 * Created by hb on 7/14/15.
 */
object fInval {

  def apply(L: Labels, r:StdRule): Boolean = r.B.forall(L.status(_) != unknown) &&
    ( r.Bp.exists(L.status(_) == out) && r.Bn.exists(L.status(_) == in) )

}
