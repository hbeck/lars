package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{StreamChoice, ch2}
import lars.core.windowfn.{WindowFunction, WindowParameters}

/**
 * Created by hb on 7/9/15.
 */
case class WindowOperator[X <: WindowParameters](wfn:WindowFunction[X], x:X, fm: Formula, ch:StreamChoice=ch2)

