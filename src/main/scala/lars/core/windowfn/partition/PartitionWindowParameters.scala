package lars.core.windowfn.partition

import lars.core.semantics.formulas.Atom
import lars.core.windowfn.WindowParameters

/**
 * Created by et on 7/13/15.
 */

case class PartitionWindowParameters(a: Set[Atom], l: Int, u: Int=0) extends WindowParameters {
  override def toString = {
    if (l != 0) {
      if (u == 0)
        ""+l
      else
        "(" + l + "," + u + ")"
    } else {
      if (u != 0)
        "+"+u
      else
        "(" + l + "," + u + ")"
    }
  }
}

object PartitionWindowParameters {
  def from(as: Set[Atom], xs: Seq[Int]) : PartitionWindowParameters = {
    val a = as
    val l = xs.head
    val u = if (xs.length == 2) xs(1) else 0
    PartitionWindowParameters(a, l,u)
  }
}