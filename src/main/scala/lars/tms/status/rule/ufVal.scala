package lars.tms.status.rule

import lars.core.semantics.programs.standard.StdRule
import lars.tms.status.{out, Labels, in}

/**
 * Created by hb on 7/14/15.
 */
object ufVal {

  def apply(L: Labels, r:StdRule): Boolean = r.Bp.forall(L.status(_) == in) &&
    r.Bn.forall(L.status(_) != in) && r.Bn.exists(L.status(_) != out)

}
