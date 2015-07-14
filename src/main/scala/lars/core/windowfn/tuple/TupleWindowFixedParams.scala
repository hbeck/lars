package lars.core.windowfn.tuple

import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by et on 7/13/15.
 */
case class TupleWindowFixedParams(x: TupleWindowParameters) extends WindowFunctionFixedParams {
  def apply(s: S, t: Int): S = TupleWindow(s,t,x)
  override def toString = x.toString
}