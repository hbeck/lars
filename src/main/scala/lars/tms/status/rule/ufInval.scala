package lars.tms.status.rule

import lars.core.semantics.programs.standard.StdRule
import lars.tms.status.Labels

/**
 * Created by hb on 7/14/15.
 */
object ufInval {

  def apply(L: Labels, r:StdRule): Boolean = !fVal(L,r) && !fInval(L,r) && !ufVal(L,r)

}
