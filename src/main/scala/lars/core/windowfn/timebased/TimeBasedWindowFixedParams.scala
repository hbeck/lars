package lars.core.windowfn.timebased

import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by hb on 7/6/15.
 */
case class TimeBasedWindowFixedParams(x: TimeBasedWindowParameters) extends WindowFunctionFixedParams /* [TimeBasedWindowParameters](x) */ {
  def apply(s: S, t: Int): S = TimeBasedWindow(s,t,x)
  override def toString = x.toString
}
