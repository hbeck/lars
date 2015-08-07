package lars.core.windowfn.time

import lars.core.semantics.formulas.WindowOperatorFixedParams

/**
 * Created by hb on 8/7/15.
 */
object WTime {
  def apply(xs: Int*) = {
    WindowOperatorFixedParams(TimeWindowFixedParams(TimeWindowParameters.from(xs)))
  }
}
