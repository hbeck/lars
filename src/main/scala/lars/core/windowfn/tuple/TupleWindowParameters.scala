package lars.core.windowfn.tuple

import lars.core.windowfn.WindowParameters

/**
 * Created by et on 7/13/15.
 */

case class TupleWindowParameters(l: Int, u: Int=0) extends WindowParameters {
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

object TupleWindowParameters {
  def from(xs: Seq[Int]) : TupleWindowParameters = {
    val l = xs.head
    val u = if (xs.length == 2) xs(1) else 0
    TupleWindowParameters(l,u)
  }
}