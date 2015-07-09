package lars.core.windowfn.time

import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by hb on 7/6/15.
 */
case class TimeWindowFixedParams(x: TimeWindowParameters) extends WindowFunctionFixedParams /* [TimeBasedWindowParameters](x) */ {
  def apply(s: S, t: Int): S = TimeWindow(s,t,x)
  override def toString = x.toString
}
