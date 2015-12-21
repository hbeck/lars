package lars.core.windowfn.time

import lars.core.semantics.formulas.WindowOperatorFixedParams
import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by hb on 7/6/15.
 */
case class TimeWindowFixedParams(x: TimeWindowParameters) extends WindowFunctionFixedParams {
  def apply(s: S, t: Int): S = TimeWindow(s,t,x)
  def toOp():WindowOperatorFixedParams = new WindowOperatorFixedParams(this)
  override def toString = x.toString
}

object TimeWindowFixedParams {
  def apply(l:Int):TimeWindowFixedParams = TimeWindowFixedParams(TimeWindowParameters(l))
  def apply(l:Int,u:Int,d:Int):TimeWindowFixedParams = TimeWindowFixedParams(TimeWindowParameters(l,u,d))
}