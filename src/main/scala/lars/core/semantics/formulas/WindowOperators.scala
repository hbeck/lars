package lars.core.semantics.formulas

import lars.core.semantics.formulas.WindowOperators.{ch2, ch1, StreamChoice}
import lars.core.semantics.streams.S
import lars.core.windowfn.{WindowFunctionFixedParams, WindowParameters, WindowFunction}

/**
 * Created by hb on 5/4/15.
 */
object WindowOperators {

  abstract class StreamChoice() extends ((S, S) => S)

  object ch1 extends StreamChoice() {
    override def apply(s1: S, s2: S) : S = s1
  }

  object ch2 extends StreamChoice() {
    override def apply(s1: S, s2: S) : S = s2
  }

}

class WindowOperator(val wfn: WindowFunctionFixedParams, val ch:StreamChoice)
case class WindowOperator1(override val wfn: WindowFunctionFixedParams) extends WindowOperator(wfn,ch1)
case class WindowOperator2(override val wfn: WindowFunctionFixedParams) extends WindowOperator(wfn,ch2) {
  override def toString = "⊞^{"+wfn+"}"
}

class WindowOperatorParam[X <: WindowParameters](wfn: WindowFunction[X], ch:StreamChoice, x:X)
//case class WindowOperator1Param[X <: WindowParameters](wfn: WindowFunctionParam[X], x:X) extends WindowOperatorParam[X](wfn,ch1,x)
//case class WindowOperator2Param[X <: WindowParameters](wfn: WindowFunctionParam[X], x:X) extends WindowOperatorParam[X](wfn,ch2,x)
