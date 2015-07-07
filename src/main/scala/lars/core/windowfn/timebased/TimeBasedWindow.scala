package lars.core.windowfn.timebased

import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunction

/**
 * Created by hb on 7/6/15.
 */
case class TimeBasedWindow(x: TimeBasedWindowParameters) extends WindowFunction /* [TimeBasedWindowParameters](x) */ {
  def apply(s: S, t: Int): S = TimeBasedWindowParam.apply(s,t,x)
  override def toString = x.toString
}
