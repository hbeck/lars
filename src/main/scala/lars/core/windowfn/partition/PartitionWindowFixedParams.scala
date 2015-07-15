package lars.core.windowfn.partition

import lars.core.semantics.streams.S
import lars.core.windowfn.WindowFunctionFixedParams

/**
 * Created by et on 7/13/15.
 */
case class PartitionWindowFixedParams(x: PartitionWindowParameters) extends WindowFunctionFixedParams {
  def apply(s: S, t: Int): S = PartitionWindow(s,t,x)
  override def toString = x.toString
}