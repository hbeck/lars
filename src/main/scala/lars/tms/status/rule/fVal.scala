package lars.tms.status.rule

import lars.core.semantics.programs.standard.StdRule
import lars.tms.status.Labels
import lars.tms.status.Status.{in, out}

/**
 * Created by hb on 7/14/15.
 */
object fVal {

  def apply(L: Labels, r:StdRule): Boolean = r.Bp.forall(L.status(_) == in) && r.Bn.forall(L.status(_) == out)

}
