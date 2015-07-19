package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{StreamChoice, ch2}
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by hb on 7/10/15.
 */
case class WindowOperatorFixedParams(wfn: WindowFunctionFixedParams, ch:StreamChoice=ch2) {
  override def toString = "⊞^{"+wfn+"}"
}
