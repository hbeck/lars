package lars.core.windowfn.time

import lars.core.windowfn.WindowParameters

/**
 * Created by hb on 5/26/15.
 */
//l: to the left (past), u: to the right (future), d: hopsize
case class TimeWindowParameters(l:Int, u:Int=0, d:Int=1) extends WindowParameters {
  override def toString = {
    if (l != 0) {
      if (u == 0 && d == 1)
        ""+l
      else if (u!=0 && d == 1)
        l + "+" + u
      else
        "(" + l + "," + u + "," + d + ")"
    } else {
      if (u != 0 && d == 1)
        "+"+u
      else
        "(" + l + "," + u + "," + d + ")"
    }
  }
}

object TimeWindowParameters {

  def from(xs: Seq[Int]) : TimeWindowParameters = {
    val l = xs(0)
    val u = if (xs.length >= 2) xs(1) else 0
    val d = if (xs.length == 3) xs(2) else 1
    TimeWindowParameters(l,u,d)
  }

}
